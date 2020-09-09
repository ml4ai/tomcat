#include "AncestralSampler.h"

namespace tomcat {
    namespace model {

        using namespace std;

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        AncestralSampler::AncestralSampler(
            shared_ptr<DynamicBayesNet> model)
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
            shared_ptr<gsl_rng> random_generator, int num_samples) {
            vector<shared_ptr<Node>> nodes;
            for (auto& node : this->model->get_nodes_topological_order()) {
                if (!dynamic_pointer_cast<RandomVariableNode>(node)
                         ->is_frozen()) {
                    nodes.push_back(node);
                }
            }

            for (auto& node : nodes) {
                this->sampled_node_labels.insert(
                    node->get_metadata()->get_label());

                const vector<shared_ptr<Node>>& parent_nodes =
                    this->model->get_parent_nodes_of(node, true);
                int real_num_samples = num_samples;
                if (node->get_metadata()->is_in_plate()) {
                    // Number of in-plate samples should match the number of
                    // data points (num_in_plate_samples). If there's no data
                    // point or just one, which will cause num_in_plate_samples
                    // to be equals to 1, then we can generate multiple samples.
                    real_num_samples = this->num_in_plate_samples == 1
                                           ? num_samples
                                           : this->num_in_plate_samples;
                }

                shared_ptr<RandomVariableNode> rv_node =
                    dynamic_pointer_cast<RandomVariableNode>(node);

                bool equal_samples = false;
                if (rv_node->get_time_step() <= this->equal_samples_time_step_limit) {
                    equal_samples = true;
                }

                Eigen::MatrixXd assignment = rv_node->sample(
                    random_generator, parent_nodes, real_num_samples, equal_samples);
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
            this->equal_samples_time_step_limit =
                equal_samples_time_step_limit;
        }

    } // namespace model
} // namespace tomcat
