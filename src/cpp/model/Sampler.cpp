#include "Sampler.h"
#include <fmt/format.h>

namespace tomcat {
    namespace model {

        Sampler::Sampler(DynamicBayesNet model,
                         std::shared_ptr<gsl_rng> random_generator)
            : random_generator(random_generator), model(model) {
            for (auto& node : model.get_nodes()) {
                this->latent_node_labels.insert(
                    node.get_metadata()->get_label());
            }
        }

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
                this->node_to_samples[node_label] =
                    Eigen::MatrixXd(num_samples, time_steps);
            }
        }

    } // namespace model
} // namespace tomcat