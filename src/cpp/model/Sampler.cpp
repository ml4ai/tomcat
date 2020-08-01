#include "Sampler.h"

#include <fmt/format.h>

namespace tomcat {
    namespace model {
        //----------------------------------------------------------------------
        // Definitions
        //----------------------------------------------------------------------

        // No definitions in this file

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        Sampler::Sampler() {}

        Sampler::Sampler(DynamicBayesNet model,
                         std::shared_ptr<gsl_rng> random_generator)
            : random_generator(random_generator), model(model) {
            for (auto& node : model.get_node_templates()) {
                this->latent_node_labels.insert(
                    node.get_metadata()->get_label());
            }
        }

        Sampler::~Sampler() {}

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void Sampler::add_data(std::string& node_label, Eigen::MatrixXd& data) {
            this->node_to_data[node_label] = data;
            this->latent_node_labels.erase(node_label);
        }

        void Sampler::add_data(std::string&& node_label,
                               Eigen::MatrixXd&& data) {
            this->node_to_data[node_label] = std::move(data);
            this->latent_node_labels.erase(node_label);
        }

        const Eigen::MatrixXd&
        Sampler::get_samples(const std::string& node_label) const {
            return this->node_to_samples.at(node_label);
        }

        void Sampler::save_samples_to_folder(
            const std::string& output_folder) const {
            // TODO
        }

        void Sampler::check_data(int num_samples) const {
            for (const auto& mapping : this->node_to_data) {
                if (mapping.second.rows() != 1 &&
                    mapping.second.rows() != num_samples) {

                    throw std::invalid_argument(fmt::format(
                        "Node {} has number of datapoints incompatible with "
                        "the number of samples to generate.",
                        mapping.first));
                }
            }
        }

        void Sampler::init_samples_matrix(int num_samples, int time_steps) {
            for (const auto& node_label : this->latent_node_labels) {
                // If there's no observation for a node in a specific time step,
                // this might be inferred by the value in the column that
                // represent such time step in the matrix of samples as it's
                // going to be filled with the original value = -1. Therefore,
                // all the matrices of samples have the same size, regardless of
                // the node's initial time step.
                this->node_to_samples[node_label] =
                    Eigen::MatrixXd::Constant(num_samples, time_steps, -1);
            }
        }

    } // namespace model
} // namespace tomcat