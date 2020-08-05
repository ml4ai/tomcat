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
        void GibbsSampler::sample(int num_samples) {
            // Filling latent nodes with a initial sampled value
            AncestralSampler initial_sampler(this->model,
                                             this->random_generator);
            for (auto& data : this->node_label_to_data) {
                std::string label = data.first;
                Tensor3 tensor = data.second;
                initial_sampler.add_data(std::move(label),
                                         std::move(data.second));
            }
            // TODO - replicate the single sample for in_plate nodes to their
            // samples matrix.
            initial_sampler.sample(1);

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

        //----------------------------------------------------------------------
        // Getters & Setters
        //----------------------------------------------------------------------

        //----------------------------------------------------------------------
        // Remove definitions
        //----------------------------------------------------------------------

        // No definitions in this file

    } // namespace model
} // namespace tomcat
