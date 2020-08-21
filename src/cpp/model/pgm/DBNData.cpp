#include "DBNData.h"

#include <boost/filesystem.hpp>

#include "../utils/Tensor3.h"

namespace tomcat {
    namespace model {

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        DBNData::DBNData() {}

        DBNData::DBNData(const std::string& data_folder_path) {
            this->init_from_folder(data_folder_path);
        }

        DBNData::~DBNData() {}

        //----------------------------------------------------------------------
        // Operator overload
        //----------------------------------------------------------------------
        const Tensor3&
            DBNData::operator[](const std::string& node_label) const {
            return this->node_label_to_data.at(node_label);
        }

        const Tensor3& DBNData::operator[](std::string&& node_label) const {
            return this->node_label_to_data.at(node_label);
        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void DBNData::init_from_folder(const std::string& data_folder_path) {
            for (const auto& file :
                 boost::filesystem::directory_iterator(data_folder_path)) {

                std::string filename = file.path().filename().string();
                std::string node_label = remove_extension(filename);
                Tensor3 data = read_tensor_from_file(file.path().string());
                this->add_data(node_label, data);
            }
        }

        std::vector<std::string> DBNData::get_node_labels() const {
            std::vector<std::string> node_labels;
            node_labels.reserve(this->node_label_to_data.size());

            for (auto& [label, data] : this->node_label_to_data) {
                node_labels.push_back(label);
            }

            return node_labels;
        }

        void DBNData::add_data(const std::string& node_label, const Tensor3& data) {
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

        bool DBNData::has_data_for(const std::string& node_label) {
            return EXISTS(node_label, this->node_label_to_data);
        }

        void DBNData::set_data_for(const std::string& node_label,
                                   const Tensor3 data) {
            if (!EXISTS(node_label, this->node_label_to_data)) {
                throw TomcatModelException("The node " + node_label +
                                           "does not belong to the DBN Data.");
            }

            this->node_label_to_data[node_label] = data;
        }

        //----------------------------------------------------------------------
        // Getters & Setters
        //----------------------------------------------------------------------
        int DBNData::get_num_data_points() const { return num_data_points; }

        int DBNData::get_time_steps() const { return time_steps; }

    } // namespace model
} // namespace tomcat
