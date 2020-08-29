#include "EvidenceSet.h"

#include <boost/filesystem.hpp>

#include "../utils/EigenExtensions.h"
#include "../utils/Tensor3.h"

namespace tomcat {
    namespace model {

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        EvidenceSet::EvidenceSet() {}

        EvidenceSet::EvidenceSet(const std::string& data_folder_path) {
            this->init_from_folder(data_folder_path);
        }

        EvidenceSet::~EvidenceSet() {}

        //----------------------------------------------------------------------
        // Operator overload
        //----------------------------------------------------------------------
        const Tensor3&
            EvidenceSet::operator[](const std::string& node_label) const {
            return this->node_label_to_data.at(node_label);
        }

        const Tensor3& EvidenceSet::operator[](std::string&& node_label) const {
            return this->node_label_to_data.at(node_label);
        }

        //----------------------------------------------------------------------
        // Static functions
        //----------------------------------------------------------------------
        Eigen::MatrixXd  EvidenceSet::get_observations_in_window(
            const Tensor3& data,
            const Eigen::VectorXd& assignment,
            int window) {
            Eigen::MatrixXd logical_data = data == assignment;

            int first_time_step = get_first_time_with_observation(data);
            int num_rows = logical_data.rows();
            int num_cols = logical_data.cols();
            int num_cols_in_window = num_cols - window + 1;
            int num_cols_with_obs = num_cols - first_time_step;
            int num_cols_with_obs_in_window =
                num_cols_in_window - first_time_step;

            // Final matrix with ones where the assignment was found in
            // any of the time steps within the window for a given time step. In
            // sum, For instance, for a given data point (row in the matrix), if
            // the window is 3 and the assignment was observed up to 2
            // (horizon - 1) time steps ahead of a current time step,
            // there will be a number one in the matrix in the
            // row for that data point in the column for that
            // time step.
            Eigen::MatrixXd logical_data_in_window =
                Eigen::MatrixXd::Constant(num_rows, num_cols_in_window, NO_OBS);

            Eigen::MatrixXd logical_data_with_obs = logical_data.block(
                0, first_time_step, num_rows, num_cols_with_obs);

            if (window == 1) {
                // When the window is equal one, the result it's just
                // the logical data previously computed. We just make
                // sure to preserve the NO_OBS values in the first
                // columns by moving a portion of the matrix only.
                logical_data_in_window.block(
                    0, first_time_step, num_rows, num_cols - first_time_step) =
                    std::move(logical_data_with_obs);
            }
            else {
                Eigen::MatrixXd cum_sum_over_time =
                    cum_sum(logical_data_with_obs, 1);

                // As the window increases, the number of columns in the final
                // matrix shrinks as for each columns we need to check window -
                // 1 subsequent columns.
                Eigen::MatrixXd obs_in_window(num_rows,
                                              num_cols_with_obs_in_window);

                for (int i = 0; i < num_rows; i++) {
                    for (int j = 0; j < num_cols_with_obs_in_window; j++) {

                        int upper_idx = window + j - 1;
                        int lower_idx = j - 1;

                        if (lower_idx < 0) {
                            obs_in_window(i, j) =
                                cum_sum_over_time(i, upper_idx);
                        }
                        else {
                            obs_in_window(i, j) =
                                cum_sum_over_time(i, upper_idx) -
                                cum_sum_over_time(i, lower_idx);
                        }
                    }
                }

                Eigen::MatrixXd ones = Eigen::MatrixXd::Ones(
                    num_rows, num_cols_with_obs_in_window);
                Eigen::MatrixXd zeros = Eigen::MatrixXd::Zero(
                    num_rows, num_cols_with_obs_in_window);

                obs_in_window = (obs_in_window.array() > 0).select(ones, zeros);

                logical_data_in_window.block(
                    0, first_time_step, num_rows, num_cols_with_obs_in_window) =
                    std::move(obs_in_window);
            }

            return logical_data_in_window;
        }

        int EvidenceSet::get_first_time_with_observation(const Tensor3& data) {

            int time_step = 0;
            auto [d1, d2, d3] = data.get_shape();
            for (int k = 0; k < d3; k++) {
                bool has_data = true;
                for (int i = 0; i < d1; i++) {
                    for (int j = 0; j < d2; j++) {
                        if (data.at(i, j, k) != NO_OBS) {
                            has_data = false;
                            time_step++;
                            break;
                        }
                    }
                    if (!has_data) {
                        break;
                    }
                }
                if (!has_data) {
                    break;
                }
            }

            return time_step;
        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void
        EvidenceSet::init_from_folder(const std::string& data_folder_path) {
            for (const auto& file :
                 boost::filesystem::directory_iterator(data_folder_path)) {

                std::string filename = file.path().filename().string();
                std::string node_label = remove_extension(filename);
                Tensor3 data = read_tensor_from_file(file.path().string());
                this->add_data(node_label, data);
            }
        }

        std::vector<std::string> EvidenceSet::get_node_labels() const {
            std::vector<std::string> node_labels;
            node_labels.reserve(this->node_label_to_data.size());

            for (auto& [label, data] : this->node_label_to_data) {
                node_labels.push_back(label);
            }

            return node_labels;
        }

        void EvidenceSet::add_data(const std::string& node_label,
                                   const Tensor3& data) {
            if (this->num_data_points == 0 && this->time_steps == 0) {
                this->num_data_points = data.get_shape()[1];
                this->time_steps = data.get_shape()[2];
            }
            else {
                if (data.get_shape()[1] != this->num_data_points) {
                    throw std::invalid_argument(
                        "The number of data points must be the same for "
                        "all the observable nodes in the folder.");
                }

                if (data.get_shape()[2] != this->time_steps) {
                    throw std::invalid_argument(
                        "The number of time steps must be the same for "
                        "all the observable nodes in the folder.");
                }
            }

            this->node_label_to_data[node_label] = data;
        }

        bool EvidenceSet::has_data_for(const std::string& node_label) const {
            return EXISTS(node_label, this->node_label_to_data);
        }

        void EvidenceSet::set_data_for(const std::string& node_label,
                                       const Tensor3 data) {
            if (!EXISTS(node_label, this->node_label_to_data)) {
                throw TomcatModelException("The node " + node_label +
                                           "does not belong to the DBN Data.");
            }

            this->node_label_to_data[node_label] = data;
        }

        void EvidenceSet::get_info(nlohmann::json& json) const {
            json["id"] = this->id;
            json["num_data_points"] = this->get_num_data_points();
            json["time_steps"] = this->get_time_steps();
        }

        Eigen::MatrixXd  EvidenceSet::get_observations_in_window_for(
            const std::string& node_label,
            const Eigen::VectorXd& assignment,
            int window) const {

            return EvidenceSet::get_observations_in_window(
                this->node_label_to_data.at(node_label), assignment, window);
        }

        //----------------------------------------------------------------------
        // Getters & Setters
        //----------------------------------------------------------------------
        int EvidenceSet::get_num_data_points() const { return num_data_points; }

        int EvidenceSet::get_time_steps() const { return time_steps; }

        void EvidenceSet::set_id(const std::string& id) { this->id = id; }

    } // namespace model
} // namespace tomcat
