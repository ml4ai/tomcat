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
        void AncestralSampler::sample(int num_samples) {
            std::vector<std::shared_ptr<RandomVariableNode>> nodes;
            for (auto& node : this->model.get_nodes_topological_order()) {
                if (!node->is_frozen()) {
                    nodes.push_back(node);
                }
            }
            this->check_data(num_samples);

            for (int s = 0; s < num_samples; s++) {
                for (auto& node : nodes) {
                    std::string node_label = node->get_metadata()->get_label();
                    if (exists(node_label, this->latent_node_labels)) {

                        this->init_samples_tensor(node_label, num_samples);
                        this->update_assignment_from_sample(node);

                        int time_step = node->get_time_step();
                        std::string label = node->get_metadata()->get_label();
                        int sample_size =
                            node->get_metadata()->get_sample_size();
                        for (int i = 0; i < sample_size; i++) {
                            double sampled_value = node->get_assignment()(i);
                            this->node_label_to_samples.at(label)(
                                i, s, time_step) = sampled_value;
                        }
                    }
                    else {
                        this->assign_data_to_node(node, s);
                    }
                }
            }
        }

    } // namespace model
} // namespace tomcat
