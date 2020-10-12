#include "AncestralSampler.h"

namespace tomcat {
    namespace model {

        using namespace std;

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        AncestralSampler::AncestralSampler(shared_ptr<DynamicBayesNet> model)
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
        void
        AncestralSampler::sample_latent(shared_ptr<gsl_rng> random_generator,
                                        int num_samples) {
            vector<shared_ptr<Node>> nodes_to_sample;

            // We start by sampling nodes in the root and keep moving forward
            // until we reach the leaves. Sampling a node depends on the values
            // sampled from its parents. Therefore, a top-down topological order
            // of the nodes is used here.
            for (auto& node : this->model->get_nodes_topological_order()) {
                shared_ptr<RandomVariableNode> rv_node =
                    dynamic_pointer_cast<RandomVariableNode>(node);

                // If a node is frozen, we don't generate samples for it as it
                // already contains pre-defined samples.
                if (!rv_node->is_frozen()) {

                    // If defined, we don't sample nodes after a given time
                    // step. Otherwise, we proceed sampling for all the time
                    // steps in the unrolled DBN.
                    if (this->max_time_step_to_sample < 0 ||
                        rv_node->get_time_step() <=
                            this->max_time_step_to_sample) {
                        nodes_to_sample.push_back(node);
                    }
                }
            }

            for (auto& node : nodes_to_sample) {
                this->sampled_node_labels.insert(
                    node->get_metadata()->get_label());
                const vector<shared_ptr<Node>>& parent_nodes =
                    this->model->get_parent_nodes_of(node, true);
                int real_num_samples = num_samples;

                if (node->get_metadata()->is_in_plate()) {
                    // Number of in-plate samples should match the number of
                    // data points (num_in_plate_samples). If no data was
                    // provided or there's just one data point was provided,
                    // which will cause num_in_plate_samples to be equals to 1,
                    // then we can generate multiple samples.
                    real_num_samples = this->num_in_plate_samples == 1
                                           ? num_samples
                                           : this->num_in_plate_samples;
                }

                shared_ptr<RandomVariableNode> rv_node =
                    dynamic_pointer_cast<RandomVariableNode>(node);

                // Check whether the samples must be the same. It's possible to
                // request samples to be equal up to a time step. That's what we
                // call homogeneous samples up to a time t.
                bool equal_samples = false;
                if (rv_node->get_time_step() <=
                    this->equal_samples_time_step_limit) {
                    equal_samples = true;
                }

                // Generate a sample for the node given its parents' assignments.
                Eigen::MatrixXd assignment = rv_node->sample(random_generator,
                                                             parent_nodes,
                                                             real_num_samples,
                                                             equal_samples);

                // A sample is stored as an assignment of the node.
                rv_node->set_assignment(assignment);
            }
        }

        void AncestralSampler::get_info(nlohmann::json& json) const {
            json["name"] = "ancestral";
        }

        //----------------------------------------------------------------------
        // Getters & Setters
        //----------------------------------------------------------------------
        void AncestralSampler::set_equal_samples_time_step_limit(
            int equal_samples_time_step_limit) {
            this->equal_samples_time_step_limit = equal_samples_time_step_limit;
        }

    } // namespace model
} // namespace tomcat
