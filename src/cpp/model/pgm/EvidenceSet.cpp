#include "EvidenceSet.h"

#include <boost/filesystem.hpp>

#include "../utils/EigenExtensions.h"
#include "../utils/Tensor3.h"

namespace tomcat {
    namespace model {

        using namespace std;

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        EvidenceSet::EvidenceSet() {}

        EvidenceSet::EvidenceSet(const string& data_folder_path) {
            this->init_from_folder(data_folder_path);
        }

        EvidenceSet::~EvidenceSet() {}

        //----------------------------------------------------------------------
        // Operator overload
        //----------------------------------------------------------------------
        const Tensor3&
            EvidenceSet::operator[](const string& node_label) const {
            return this->node_label_to_data.at(node_label);
        }

        const Tensor3& EvidenceSet::operator[](string&& node_label) const {
            return this->node_label_to_data.at(node_label);
        }

        //----------------------------------------------------------------------
        // Static functions
        //----------------------------------------------------------------------
        Eigen::MatrixXd EvidenceSet::get_observations_in_window(
            const Tensor3& data,
            const Eigen::VectorXd& assignment,
            int window) {

            Eigen::MatrixXd logical_data = (data == assignment);
            int first_time_step = get_first_time_with_observation(data);
            int num_rows = logical_data.rows();
            int num_cols = logical_data.cols();
            // Replace the first columns with no observable data with NO_OBS
            // value so it can be preserved in the operations below.
            logical_data.block(0, 0, num_rows, first_time_step) =
                Eigen::MatrixXd::Constant(num_rows, first_time_step, NO_OBS);

            Eigen::MatrixXd logical_data_in_window(num_rows, num_cols - window);
            if (window == 0) {
                logical_data_in_window = logical_data;
            }
            else {
                Eigen::MatrixXd cum_sum_over_time = cum_sum(logical_data, 1);

                Eigen::MatrixXd num_obs_in_window(num_rows, num_cols - window);
                int j = 0;
                for (int w = window; w < cum_sum_over_time.cols(); w++) {
                    num_obs_in_window.col(j) =
                        cum_sum_over_time.col(w).array() -
                        cum_sum_over_time.col(j).array();
                    j++;
                }

                Eigen::MatrixXd ones = Eigen::MatrixXd::Ones(
                    num_obs_in_window.rows(), num_obs_in_window.cols());
                Eigen::MatrixXd zeros = Eigen::MatrixXd::Zero(
                    num_obs_in_window.rows(), num_obs_in_window.cols());
                Eigen::MatrixXd no_obs = Eigen::MatrixXd::Constant(
                    num_obs_in_window.rows(), num_obs_in_window.cols(), NO_OBS);
                no_obs = (num_obs_in_window.array() < 0).select(no_obs, zeros);
                logical_data_in_window =
                    (num_obs_in_window.array() > 0).select(ones, no_obs);
            }

            return logical_data_in_window;
        }

        int EvidenceSet::get_first_time_with_observation(const Tensor3& data) {

            int time_step = -1;
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
        EvidenceSet::init_from_folder(const string& data_folder_path) {
            for (const auto& file :
                 boost::filesystem::directory_iterator(data_folder_path)) {

                string filename = file.path().filename().string();
                string node_label = remove_extension(filename);
                Tensor3 data = read_tensor_from_file(file.path().string());
                this->add_data(node_label, data);
            }
        }

        vector<string> EvidenceSet::get_node_labels() const {
            vector<string> node_labels;
            node_labels.reserve(this->node_label_to_data.size());

            for (auto& [label, data] : this->node_label_to_data) {
                node_labels.push_back(label);
            }

            return node_labels;
        }

        void EvidenceSet::add_data(const string& node_label,
                                   const Tensor3& data) {
            if (this->num_data_points == 0 && this->time_steps == 0) {
                this->num_data_points = data.get_shape()[1];
                this->time_steps = data.get_shape()[2];
            }
            else {
                if (data.get_shape()[1] != this->num_data_points) {
                    throw invalid_argument(
                        "The number of data points must be the same for "
                        "all the observable nodes in the folder.");
                }

                if (data.get_shape()[2] != this->time_steps) {
                    throw invalid_argument(
                        "The number of time steps must be the same for "
                        "all the observable nodes in the folder.");
                }
            }

            this->node_label_to_data[node_label] = data;
        }

        bool EvidenceSet::has_data_for(const string& node_label) const {
            return EXISTS(node_label, this->node_label_to_data);
        }

        void EvidenceSet::set_data_for(const string& node_label,
                                       const Tensor3 data) {
            if (!EXISTS(node_label, this->node_label_to_data)) {
                throw TomcatModelException("The node " + node_label +
                                           "does not belong to the DBN Data.");
            }

            this->node_label_to_data[node_label] = data;
        }

        Eigen::MatrixXd EvidenceSet::get_observations_in_window_for(
            const string& node_label,
            const Eigen::VectorXd& assignment,
            int window) const {

            return EvidenceSet::get_observations_in_window(
                this->node_label_to_data.at(node_label), assignment, window);
        }

        void EvidenceSet::keep_first(int num_samples) {
            for(auto&[node_label, data] : this->node_label_to_data){
                data = data.slice(0, num_samples, 1);
            }
            this->num_data_points = num_samples;
        }

        void EvidenceSet::shrink_up_to(int time_step) {
            for(auto&[node_label, data] : this->node_label_to_data){
                data = data.slice(0, time_step + 1, 2);
            }
            this->time_steps = time_step + 1;
        }

        //----------------------------------------------------------------------
        // Getters & Setters
        //----------------------------------------------------------------------
        int EvidenceSet::get_num_data_points() const { return num_data_points; }

        int EvidenceSet::get_time_steps() const { return time_steps; }

    } // namespace model
} // namespace tomcat
