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
            std::shared_ptr<DynamicBayesNet> model)
            : Sampler(model) {}

        AncestralSampler::~AncestralSampler() {}

        //----------------------------------------------------------------------
        // Copy & Move constructors/assignments
        //----------------------------------------------------------------------
        AncestralSampler&
        AncestralSampler::operator=(const AncestralSampler& sampler) {
            this->copy_sampler(sampler);
            return *this;
        }

        AncestralSampler::AncestralSampler(const AncestralSampler& sampler) {
            this->copy_sampler(sampler);
        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void AncestralSampler::sample_latent(
            std::shared_ptr<gsl_rng> random_generator, int num_samples) {
            std::vector<std::shared_ptr<Node>> nodes;
            for (auto& node : this->model->get_nodes_topological_order()) {
                if (!std::dynamic_pointer_cast<RandomVariableNode>(node)
                         ->is_frozen()) {
                    nodes.push_back(node);
                }
            }

            for (auto& node : nodes) {
                this->sampled_node_labels.insert(
                    node->get_metadata()->get_label());

                const std::vector<std::shared_ptr<Node>>& parent_nodes =
                    this->model->get_parent_nodes_of(node, true);
                if (node->get_metadata()->is_in_plate()) {
                    num_samples = this->num_in_plate_samples == 1
                                      ? num_samples
                                      : this->num_in_plate_samples;
                }

                std::shared_ptr<RandomVariableNode> rv_node =
                    std::dynamic_pointer_cast<RandomVariableNode>(node);
                Eigen::MatrixXd assignment = rv_node->sample(
                    random_generator, parent_nodes, num_samples);
                rv_node->set_assignment(assignment);
            }
        };

    } // namespace model
} // namespace tomcat