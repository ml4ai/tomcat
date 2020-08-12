#include "Sampler.h"

#include <array>

#include <fmt/format.h>

#include "FileHandler.h"

namespace tomcat {
    namespace model {
        //----------------------------------------------------------------------
        // Definitions
        //----------------------------------------------------------------------

#define exists(member, container) (container.find(member) != container.end())

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        Sampler::Sampler() {}

        Sampler::Sampler(DynamicBayesNet model,
                         std::shared_ptr<gsl_rng> random_generator)
            : random_generator(random_generator), model(model) {
        }

        Sampler::~Sampler() {}

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void Sampler::sample(int num_samples) {
            this->freeze_observable_nodes();
            this->sample_latent(num_samples);
            this->unfreeze_observable_nodes();
        }

        void Sampler::add_data(const std::string& node_label, Tensor3& data) {
            // If data has been previously assigned for some node, all the
            // subsequent data assignments must have the same number of data
            // points.
            if (this->num_data_points == 0 ||
                data.get_shape()[2] == this->num_data_points) {
                this->num_data_points = data.get_shape()[1];

                for (auto& node : this->model.get_nodes_by_label(node_label)) {
                    node->set_assignment(
                        data(node->get_time_step(), 2).transpose());
                    this->observable_node_labels.insert(node_label);
                }
            }
            else {
                throw std::invalid_argument(
                    "The number of data points must be the same for all "
                    "observable nodes.");
            }
        }

        Tensor3 Sampler::get_samples(const std::string& node_label) const {
            std::vector<std::shared_ptr<RandomVariableNode>> nodes =
                this->model.get_nodes_by_label(node_label);
            if (!nodes.empty()) {
                int d1 = nodes[0]->get_metadata()->get_sample_size();
                int d2 = nodes[0]->get_size();
                int d3 = this->model.get_time_steps();
                double* buffer = new double[d1 * d2 * d3];
                std::fill_n(buffer, d1 * d2 * d3, -1);
                for (auto& node : nodes) {
                    Eigen::Matrix assignment = node->get_assignment();
                    for(int i = 0; i < assignment.rows(); i++){
                        for(int j = 0; j < assignment.cols(); j++){
                            int index = j*d2*d3 + i*d3 + node->get_time_step();
                            buffer[index] = assignment(i, j);
                        }
                    }
                }

                return Tensor3(buffer, d1, d2, d3);
            }
            else {
                throw std::invalid_argument(
                    "This node does not belong to the model.");
            }

        }

        void Sampler::save_samples_to_folder(
            const std::string& output_folder) const {
            for (const auto& node_label : this->sampled_node_labels) {
                std::string filename = node_label + ".txt";
                std::string filepath = get_filepath(output_folder, filename);
                save_tensor_to_file(filepath, this->get_samples(node_label));
            }
        }

        void Sampler::freeze_observable_nodes() {
            for(const auto& node_label : this->observable_node_labels) {
                for (auto& node : this->model.get_nodes_by_label(node_label)) {
                    node->freeze();
                }
            }
        }

        void Sampler::unfreeze_observable_nodes() {
            for(const auto& node_label : this->observable_node_labels) {
                for (auto& node : this->model.get_nodes_by_label(node_label)) {
                    node->unfreeze();
                }
            }
        }

    } // namespace model
} // namespace tomcat