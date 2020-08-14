#include "GibbsSampler.h"

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
        GibbsSampler::GibbsSampler(DynamicBayesNet model,
                                   std::shared_ptr<gsl_rng> random_generator,
                                   int burn_in_period)
            : Sampler(model, random_generator), burn_in_period(burn_in_period) {
        }

        GibbsSampler::~GibbsSampler() {}

        //----------------------------------------------------------------------
        // Copy & Move constructors/assignments
        //----------------------------------------------------------------------
        GibbsSampler& GibbsSampler::operator=(const GibbsSampler& sampler) {
            this->model = sampler.model;
            this->burn_in_period = sampler.burn_in_period;
            return *this;
        }

        GibbsSampler::GibbsSampler(const GibbsSampler& sampler) {
            this->model = sampler.model;
            this->burn_in_period = sampler.burn_in_period;
        }

        //----------------------------------------------------------------------
        // Operator overload
        //----------------------------------------------------------------------

        //----------------------------------------------------------------------
        // Static functions
        //----------------------------------------------------------------------

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void GibbsSampler::sample_latent(int num_samples) {
//            // This needs to be repeated for num_samples + burn_in
//            // Samples generated here need to be saved in a structure apart and at the end they can be placed as nodes assignments.
//            // Remember to clear the unfrozen nodes assignments before starting
//
//            // We will proceed from the leaves to the root
//            std::vector<std::shared_ptr<RandomVariableNode>> nodes;
//            for (auto& node : this->model.get_nodes_topological_order(false)) {
//                if (!node->is_frozen()) {
//                    nodes.push_back(node);
//                }
//            }
//
//            this->fill_initial_samples();
//            for (auto& node : nodes) {
//                if (node->get_metadata()->is_parameter()) {
//                    // As child nodes are processed, the sufficient statistic
//                    // table of their dependent parent parameter nodes are
//                    // updated accordingly. So at this point we already have all
//                    // the information needed to sample the parameter from its
//                    // posterior.
//                    node.sample_from_posterior(random_generator);
//                }
//                else {
//                    std::vector<std::shared_ptr<RandomVariableNode>>
//                        child_nodes = this->model.get_child_nodes_of(*node);
//
//                    Eigen::MatrixXd weights = Eigen::MatrixXd::Zero(
//                        node->get_size(),
//                        node->get_metadata()->get_cardinality());
//                    for (auto& child_node : child_nodes) {
//                        for (int i = 0;
//                             i < node->get_metadata()->get_cardinality();
//                             i++) {
//                            node->set_assignment(Eigen::MatrixXd::Constant(
//                                node->get_size(), 1, i));
//                            Eigen::VectorXd pdfs = child_node->get_pdfs(
//                                random_generator,
//                                this->model.get_parent_nodes_of(*child_node,
//                                                                true));
//                            weights.col(i) = weights.col(i).array() +
//                                             (pdfs.array() + EPSILON).log();
//                            // Get pdf for parents given each one of the parents
//                            // assignment
//                        }
//                    }
//
//                    // Unlog the weights
//                    weights = weights.array() - weights.maxCoeff();
//                    weights = weights.array().exp() - EPSILON;
//
//                    std::vector<std::shared_ptr<RandomVariableNode>>
//                        parent_nodes =
//                            this->model.get_parent_nodes_of(*node, true);
//
//                    // Similar to the sample method. We'll iterate over each one
//                    // of the weights and call the sample function with weight
//                    // that already exist in the Distribution class
//                    node->sample(random_generator,
//                                 parent_nodes,
//                                 node->get_size(),
//                                 weights);
//
//                    // Get each one of the distributions given the parents'
//                    // assignments and update the suff statistics of the node
//                    // (and the node will do the same for the cpd) by setting
//                    // the each one of the node's assignments to a counting
//                    // table or list value in case of a gaussian.
//                    // At this point do not worry about parameter that depend
//                    // on another parameter.
//                    node->update_parents_sufficient_statistics(parent_nodes);
//                }
//            }
        }

        void GibbsSampler::fill_initial_samples() {
            AncestralSampler initial_sampler(this->model,
                                             this->random_generator);
            initial_sampler.sample(1);
        }

        //----------------------------------------------------------------------
        // Getters & Setters
        //----------------------------------------------------------------------

        //----------------------------------------------------------------------
        // Remove definitions
        //----------------------------------------------------------------------

        // No definitions in this file

    } // namespace model
} // namespace tomcat
