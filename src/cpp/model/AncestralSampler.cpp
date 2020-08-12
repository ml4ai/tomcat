#include "AncestralSampler.h"

namespace tomcat {
    namespace model {

        //----------------------------------------------------------------------
        // Definitions
        //----------------------------------------------------------------------
#define exists(member, container) container.find(member) != container.end()

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        AncestralSampler::AncestralSampler(
            DynamicBayesNet model, std::shared_ptr<gsl_rng> random_generator)
            : Sampler(model, random_generator) {}

        AncestralSampler::~AncestralSampler() {}

        //----------------------------------------------------------------------
        // Copy & Move constructors/assignments
        //----------------------------------------------------------------------
        AncestralSampler&
        AncestralSampler::operator=(const AncestralSampler& sampler) {
            this->model = sampler.model;
            return *this;
        }

        AncestralSampler::AncestralSampler(const AncestralSampler& sampler) {
            this->model = sampler.model;
        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void AncestralSampler::sample_latent(int num_samples) {
            std::vector<std::shared_ptr<RandomVariableNode>> nodes;
            for (auto& node : this->model.get_nodes_topological_order()) {
                if (!node->is_frozen()) {
                    nodes.push_back(node);
                }
            }

            for (int s = 0; s < num_samples; s++) {
                for (auto& node : nodes) {
                    this->sampled_node_labels.insert(
                        node->get_metadata()->get_label());

                    const std::vector<std::shared_ptr<RandomVariableNode>>&
                        parent_nodes =
                            this->model.get_parent_nodes_of(*node, true);
                    int num_samples =
                        this->num_data_points == 0 ? 1 : this->num_data_points;
                    Eigen::MatrixXd assignment = node->sample(
                        this->random_generator, parent_nodes, num_samples);
                    node->set_assignment(assignment);
                }
            }
        };

    } // namespace model
} // namespace tomcat
