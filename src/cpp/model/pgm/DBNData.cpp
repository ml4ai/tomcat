#include "EvidenceSet.h"

#include <boost/filesystem.hpp>

#include "Tensor3.h"

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

        void EvidenceSet::add_data(std::string node_label, Tensor3 data) {
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

        //----------------------------------------------------------------------
        // Getters & Setters
        //----------------------------------------------------------------------
        int EvidenceSet::get_num_data_points() const { return num_data_points; }

        int EvidenceSet::get_time_steps() const { return time_steps; }

    } // namespace model
} // namespace tomcat