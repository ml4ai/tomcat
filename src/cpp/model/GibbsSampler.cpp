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
            this->fill_initial_samples();

//            std::vector<std::shared_ptr<RandomVariableNode>> in_plate_nodes;
//            std::vector<std::shared_ptr<RandomVariableNode>> not_in_plate_nodes;
//
//            for (auto& node : this->model.get_nodes_topological_order()) {
//                std::string node_label =
//                    node->get_metadata()->get_label();
//                exists(node_label, this->latent_node_labels) {
//                    this->init_samples_tensor(node_label, num_samples);
//                }
//
//                if (node->get_metadata()->is_in_plate()) {
//                    in_plate_nodes.push_back(node);
//                }
//                else {
//                    not_in_plate_nodes.push_back(node);
//                }
//            }
//
//            int num_in_plate_copies =
//                this->num_data_points == 0 ? 1 : this->num_data_points;
//            for (int data_idx = 0; data_idx < num_in_plate_copies; data_idx++) {
//                if (this->num_data_points > 1) {
//                    // When the number of data points is greater than 0, it
//                    // means data was provided for some in-plate nodes and,
//                    // therefore, we have to sample in-plate latent nodes for
//                    // each copy. As a node only comports one assigned value at
//                    // a time, we need to assign the correct value to a node
//                    // before proceeding.
//                    for (auto& node : in_plate_nodes) {
//                        std::string node_label =
//                            node->get_metadata()->get_label();
//                        if (exists(node_label, this->latent_node_labels)) {
//                            int t = node->get_time_step();
//                            Eigen::VectorXd assignment =
//                                this->get_samples(node_label)(data_idx, t);
//                            node->set_assignment(assignment);
//                        }
//                        else {
//                            this->assign_data_to_node(node, data_idx);
//                        }
//                    }
//
//                    for (auto& node : in_plate_nodes) {
//                        std::string node_label =
//                            node->get_metadata()->get_label();
//                        if (exists(node_label, this->latent_node_labels)) {
//                            Eigen::VectorXd assignment = node->sample_from_posterior();
//                            node->set_assignment(assignment);
//
//                            int time_step = node->get_time_step();
//                            int sample_size =
//                                node->get_metadata()->get_sample_size();
//                            for (int i = 0; i < sample_size; i++) {
//                                double sampled_value = assignment(i);
//                                this->node_label_to_samples.at(node_label)(
//                                    i, data_idx, time_step) = sampled_value;
//                            }
//
//                            // TODO:
//                            // 1. For each parent node, include link of other parents and this cpd.
//                            // 2. For each parameter node, update sufficient statistics
//
//                        }
//                    }
//                }
//            }

            // TODO -
            //  1. For nodes that are not in plate, compute the posterior using
            //  the sufficient statistics
            //  2. Otherwise, populate data with samples for the node at each
            //  data point
            //  I am gonna use the samples matrix of in_plate nodes t=in the
            //  computations instead of individual assignments. I can loop over
            //  it assigning values to the parent nodes and sampling from the
            //  node to get each row of the samples matrix for a node.

            //            std::vector<std::shared_ptr<RandomVariableNode>>
            //            nodes; for (auto& node :
            //            this->model.get_nodes_topological_order()) {
            //                if (!node->is_frozen()) {
            //                    nodes.push_back(node);
            //                }
            //            }
            //            this->check_data(num_samples);
            //
            //            for (int s = 0; s < num_samples; s++) {
            //                for (auto& node : nodes) {
            //                    std::string node_label =
            //                    node->get_metadata()->get_label(); if
            //                    (exists(node_label, this->latent_node_labels))
            //                    {
            //
            //                        this->init_samples_tensor(node_label,
            //                        num_samples);
            //                        this->update_assignment_from_sample(node);
            //
            //                        int time_step = node->get_time_step();
            //                        std::string label =
            //                        node->get_metadata()->get_label(); int
            //                        sample_size =
            //                            node->get_metadata()->get_sample_size();
            //                        for (int i = 0; i < sample_size; i++) {
            //                            double sampled_value =
            //                            node->get_assignment()(i);
            //                            this->node_label_to_samples.at(label)(
            //                                i, s, time_step) = sampled_value;
            //                        }
            //                    }
            //                    else {
            //                        this->assign_data_to_node(node, s);
            //                    }
            //                }
            //            }
        }

        void GibbsSampler::fill_initial_samples() {
//            AncestralSampler initial_sampler(this->model,
//                                             this->random_generator);
//            for (auto& data : this->node_label_to_data) {
//                std::string label = data.first;
//                Tensor3 tensor = data.second;
//                initial_sampler.add_data(std::move(label),
//                                         std::move(data.second));
//            }
//
//            // This is going to fill the nodes' assignments with sampled values.
//            initial_sampler.sample(1);
//
//            // For in-plate nodes, we need to generate as many samples as the
//            // number of data points. So we freeze non-in-plate nodes and
//            // generate more samples for the in-plate nodes.
//            std::vector<std::shared_ptr<RandomVariableNode>> nodes_to_unfreeze;
//            for (auto& node : this->model.get_nodes()) {
//                if (!node->get_metadata()->is_in_plate()) {
//                    // A node can be previously frozen (in a pre-trained model,
//                    // for instance) and we don't want to unfreeze it in this
//                    // process.
//                    if (!node->is_frozen()) {
//                        node->freeze();
//                        nodes_to_unfreeze.push_back(node);
//                    }
//                }
//            }
//
//            // This will generate samples for all in-plate latent nodes given
//            // data and previously sampled values for non-in-plate nodes.
//            initial_sampler.sample(this->num_data_points);
//
//            for (auto& node : nodes_to_unfreeze) {
//                node->unfreeze();
//            }
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
