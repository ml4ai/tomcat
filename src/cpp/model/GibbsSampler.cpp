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
            // This needs to be repeated for num_samples + burn_in
            // Samples generated here need to be saved in a structure apart and
            // at the end they can be placed as nodes assignments. Remember to
            // clear the unfrozen nodes assignments before starting

            // We will proceed from the leaves to the root
            std::vector<std::shared_ptr<Node>> data_nodes;
            std::vector<std::shared_ptr<Node>> parameter_nodes;
            for (auto& node : this->model.get_nodes_topological_order()) {
                if (!std::dynamic_pointer_cast<RandomVariableNode>(node)
                         ->is_frozen()) {
                    if (node->get_metadata()->is_parameter()) {
                        parameter_nodes.push_back(node);
                    }
                    else {
                        data_nodes.push_back(node);
                    }
                }
            }

            this->fill_initial_samples();

            for (int i = 0; i < this->burn_in_period; i++) {
                for (auto& node : data_nodes) {
                    this->sample_data_node(node, true);
                }

                for (auto& node : parameter_nodes) {
                    this->sample_parameter_node(node, true);
                }
            }

            for (int i = 0; i < num_samples; i++) {
                for (auto& node : data_nodes) {
                    this->sample_data_node(node, false);
                }

                for (auto& node : parameter_nodes) {
                    this->sample_parameter_node(node, false);
                }
            }
        }

        void GibbsSampler::sample_data_node(std::shared_ptr<Node> node,
                                            bool discard) {

            Eigen::MatrixXd weights = this->get_weights_for(node);
            std::vector<std::shared_ptr<Node>> parent_nodes =
                this->model.get_parent_nodes_of(node, true);
            std::shared_ptr<RandomVariableNode> rv_node =
                std::dynamic_pointer_cast<RandomVariableNode>(node);

            Eigen::MatrixXd sample = rv_node->sample(
                random_generator, parent_nodes, node->get_size(), weights);

            rv_node->update_parents_sufficient_statistics(parent_nodes);
            if (sample.rows() == 1) {
                // TODO - Store sample only for nodes with single assignment
            }
        }

        Eigen::MatrixXd
        GibbsSampler::get_weights_for(const std::shared_ptr<Node>& node) {
            std::vector<std::shared_ptr<Node>> child_nodes =
                this->model.get_child_nodes_of(node);
            Eigen::MatrixXd weights = Eigen::MatrixXd::Zero(
                node->get_size(), node->get_metadata()->get_cardinality());

            for (auto& child_node : child_nodes) {
                for (int i = 0; i < node->get_metadata()->get_cardinality();
                     i++) {

                    std::dynamic_pointer_cast<RandomVariableNode>(node)
                        ->set_assignment(
                            Eigen::MatrixXd::Constant(node->get_size(), 1, i));
                    Eigen::VectorXd pdfs =
                        std::dynamic_pointer_cast<RandomVariableNode>(
                            child_node)
                            ->get_pdfs(random_generator,
                                       this->model.get_parent_nodes_of(
                                           child_node, true));

                    if (node->get_size() == 1) {
                        // If an out-of-plate node is parent of an in-plate
                        // node, the former will have a single assignment at a
                        // time and the latter multiple ones (as many as the
                        // number of data points passed in the observed nodes).
                        // In this case, each in-plate instance is treated as a
                        // child of the out-of-plate node and therefore, their
                        // pdfs must be multiplied instead of stored
                        // separately as a row in the matrix of weights.
                        double cum_log_pdf =
                            (pdfs.array() + EPSILON).log().sum();
                        weights(0, i) = weights(0, i) + cum_log_pdf;
                    }
                    else {
                        weights.col(i) = weights.col(i).array() +
                                         (pdfs.array() + EPSILON).log();
                    }
                }
            }

            // Unlog the weights
            weights = weights.array() - weights.maxCoeff();
            weights = weights.array().exp() - EPSILON;

            return weights;
        }

        void GibbsSampler::sample_parameter_node(std::shared_ptr<Node> node,
                                                 bool discard) {
            // As nodes are processed, the sufficient statistic
            // table of their dependent parent parameter nodes are
            // updated accordingly. So at this point we already have all
            // the information needed to sample the parameter from its
            // posterior.
//            std::dynamic_pointer_cast<RandomVariableNode>(node)
//                .sample_from_posterior(random_generator);
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
