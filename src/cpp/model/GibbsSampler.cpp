#include "GibbsSampler.h"

#include "AncestralSampler.h"

namespace tomcat {
    namespace model {

        //----------------------------------------------------------------------
        // Definitions
        //----------------------------------------------------------------------

#define exists(member, container) (container.find(member) != container.end())

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
            // We will proceed from the root to the leaves so that child nodes
            // can update the sufficient statistics of parameter nodes correctly
            // given that parent nodes were already sampled.
            this->reset();
            std::vector<std::shared_ptr<Node>> data_nodes;
            std::vector<std::shared_ptr<Node>> parameter_nodes;
            std::vector<std::shared_ptr<Node>> latent_nodes;
            for (auto& node : this->model.get_nodes_topological_order()) {
                if (node->get_metadata()->is_parameter()) {
                    if (!std::dynamic_pointer_cast<RandomVariableNode>(node)
                             ->is_frozen()) {
                        // TODO - change this if we need to have a parameter
                        // that depends on other parameters.
                        parameter_nodes.push_back(node);
                        latent_nodes.push_back(node);
                    }
                }
                else {
                    // Observable nodes (frozen) need to be processed to update
                    // the sufficient statistics of the distribution they depend
                    // on.
                    data_nodes.push_back(node);
                    latent_nodes.push_back(node);
                }
            }

            this->fill_initial_samples();
            this->init_samples_storage(num_samples, latent_nodes);

            for (int i = 0; i < this->burn_in_period + num_samples; i++) {
                bool discard = i < burn_in_period;
                for (auto& node : data_nodes) {
                    this->sample_data_node(node, discard);
                }

                for (auto& node : parameter_nodes) {
                    this->sample_parameter_node(node, discard);
                }

                if(i >= this->burn_in_period){
                    this->iteration++;
                }
            }
        }

        void GibbsSampler::reset(){
            this->iteration = 0;
            this->node_label_to_samples.clear();
        }

        void GibbsSampler::fill_initial_samples() {
            // Observable nodes are already frozen by the gibbs sampler thus
            // there's no need to add them as data in the ancestral sampler.
            AncestralSampler initial_sampler(this->model,
                                             this->random_generator);
            initial_sampler.set_num_in_plate_samples(
                this->num_in_plate_samples);
            initial_sampler.sample(1);
        }

        void GibbsSampler::init_samples_storage(
            int num_samples, std::vector<std::shared_ptr<Node>> latent_nodes) {
            // If there's no observation for a node in a specific time step,
            // this might be inferred by the value in the column that
            // represent such time step in the matrix of samples as it's
            // going to be filled with the original value = -1. Therefore,
            // all the matrices of samples have the same size, regardless of
            // the node's initial time step.
            for (const auto& node : latent_nodes) {
                std::string node_label = node->get_metadata()->get_label();
                if (!exists(node_label, this->node_label_to_samples)) {
                    int sample_size = node->get_metadata()->get_sample_size();
                    this->node_label_to_samples[node_label] =
                        Tensor3::constant(sample_size,
                                          num_samples,
                                          this->model.get_time_steps(),
                                          -1);
                }
            }
        }

        void GibbsSampler::sample_data_node(std::shared_ptr<Node> node, bool discard) {

            Eigen::MatrixXd weights = this->get_weights_for(node);
            std::vector<std::shared_ptr<Node>> parent_nodes =
                this->model.get_parent_nodes_of(node, true);
            std::shared_ptr<RandomVariableNode> rv_node =
                std::dynamic_pointer_cast<RandomVariableNode>(node);

            if (!rv_node->is_frozen()) {
                Eigen::MatrixXd sample = rv_node->sample(
                    random_generator, parent_nodes, node->get_size(), weights);
                rv_node->set_assignment(sample);
                if (!discard) {
                    this->keep_sample(rv_node, sample);
                }
            }

            rv_node->update_parents_sufficient_statistics(parent_nodes);
            rv_node->reset_sufficient_statistics();
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

        void GibbsSampler::keep_sample(
            const std::shared_ptr<RandomVariableNode>& node,
            const Eigen::MatrixXd& sample) {
            if (sample.rows() == 1) {
                std::string node_label = node->get_metadata()->get_label();
                int time_step = node->get_time_step();
                for (int i = 0; i < sample.cols(); i++) {
                    this->node_label_to_samples.at(node_label)(
                        i, this->iteration, time_step) =
                        sample(0, i);
                }
            }
        }

        void GibbsSampler::sample_parameter_node(std::shared_ptr<Node> node,
                                                 bool discard) {
            // As nodes are processed, the sufficient statistic
            // table of their dependent parent parameter nodes are
            // updated accordingly. So at this point we already have all
            // the information needed to sample the parameter from its
            // posterior.
            std::vector<std::shared_ptr<Node>> parent_nodes =
                this->model.get_parent_nodes_of(node, true);
            std::shared_ptr<RandomVariableNode> rv_node =
                std::dynamic_pointer_cast<RandomVariableNode>(node);
            Eigen::MatrixXd sample = rv_node->sample_from_conjugacy(
                random_generator, parent_nodes, rv_node->get_size());
            rv_node->set_assignment(sample);

            if (!discard) {
                this->keep_sample(rv_node, sample);
            }
        }

        Tensor3 GibbsSampler::get_samples(const std::string &node_label) const {
            return this->node_label_to_samples.at(node_label);
        }

    } // namespace model
} // namespace tomcat
