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
            for (auto& node : model.get_node_templates()) {
                std::string node_label = node.get_metadata()->get_label();
                this->latent_node_labels.insert(node_label);
                this->node_label_to_metadata[node_label] =
                    *(node.get_metadata());
            }
        }

        Sampler::~Sampler() {}

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void Sampler::add_data(std::string& node_label, Tensor3& data) {
            this->node_label_to_data[node_label] = data;
            this->latent_node_labels.erase(node_label);
        }

        void Sampler::add_data(std::string&& node_label, Tensor3&& data) {
            this->node_label_to_data[node_label] = std::move(data);
            this->latent_node_labels.erase(node_label);
        }

        const Tensor3&
        Sampler::get_samples(const std::string& node_label) const {
            return this->node_label_to_samples.at(node_label);
        }

        void Sampler::save_samples_to_folder(
            const std::string& output_folder) const {

            for (const auto& mapping : this->node_label_to_samples) {
                std::string filename = mapping.first + ".txt";
                std::string filepath = get_filepath(output_folder, filename);
                save_tensor_to_file(filepath, mapping.second);
            }
        }

        void Sampler::check_data(int num_samples) const {
            for (const auto& mapping : this->node_label_to_data) {
                std::array<int, 3> tensor_shape = mapping.second.get_shape();
                if (tensor_shape[1] != 1 && tensor_shape[1] != num_samples) {

                    throw std::invalid_argument(fmt::format(
                        "Node {} has number of datapoints incompatible with "
                        "the number of samples to generate.",
                        mapping.first));
                }
            }
        }

        void Sampler::init_samples_tensor(const std::string& node_label,
                                          int num_samples) {
            // If there's no observation for a node in a specific time step,
            // this might be inferred by the value in the column that
            // represent such time step in the matrix of samples as it's
            // going to be filled with the original value = -1. Therefore,
            // all the matrices of samples have the same size, regardless of
            // the node's initial time step.
            if (!exists(node_label, this->node_label_to_samples)) {
                int sample_size = this->node_label_to_metadata.at(node_label)
                                      .get_sample_size();
                this->node_label_to_samples[node_label] = Tensor3::constant(
                    sample_size, num_samples, this->model.get_time_steps(), -1);
            }
        }

        void Sampler::update_assignment_from_sample(
            std::shared_ptr<RandomVariableNode> node) {
            const std::vector<std::shared_ptr<RandomVariableNode>>&
                parent_nodes = this->model.get_parent_nodes_of(*node, true);
            Eigen::VectorXd assignment =
                node->sample(this->random_generator, parent_nodes);
            node->set_assignment(assignment);
        }

        void Sampler::assign_data_to_node(
            const std::shared_ptr<RandomVariableNode>& node,
            int data_point_index) {

            std::string label = node->get_metadata()->get_label();

            if (exists(label, this->node_label_to_data)) {
                Eigen::VectorXd assignment = this->node_label_to_data[label](
                    data_point_index, node->get_time_step());
                node->set_assignment(assignment);
            }
        }

    } // namespace model
} // namespace tomcat