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
        void AncestralSampler::sample(int num_samples, int time_steps) {
            this->model.unroll(time_steps, false);
            std::vector<std::shared_ptr<RandomVariableNode>> nodes =
                this->model.get_nodes_topological_order();

            this->check_data(num_samples);
            this->init_samples_matrix(num_samples, time_steps);

            for (int s = 0; s < num_samples; s++) {
                for (const auto& node : nodes) {
                    if (exists(node->get_metadata()->get_label(),
                               this->latent_node_labels)) {

                        // TODO - The 2 instructions below can possibly be moved
                        //  to the Sampler class. Check if Gibbs will use it.
                        const std::vector<std::shared_ptr<RandomVariableNode>>&
                            parent_nodes =
                                this->model.get_parent_nodes_of(*node, true);
                        Eigen::VectorXd assignment =
                            node->sample(this->random_generator, parent_nodes);
                        node->set_assignment(assignment);

                        // TODO - fix for multidimensional sample size (e.g.
                        //  samples from a parameter with dirichlet prior)
                        this->node_to_samples.at(
                            node->get_metadata()->get_label())(
                            s, node->get_time_step()) =

                            static_cast<int>(assignment(0));
                    }
                    else {
                        this->assign_data_to_node(node, s);
                    }
                }
            }
        }

        void AncestralSampler::assign_data_to_node(
            const std::shared_ptr<RandomVariableNode>& node,
            int data_point_index) {

            std::string node_label = node->get_metadata()->get_label();

            if (exists(node_label, this->node_to_data)) {
                // TODO - extend this to multidimensional sample size. I am
                //  assuming the samples are 1D numbers but this is not true for
                //  all of the distributions. A sample from a Dirichlet or a
                //  multivariate Gaussian can yield a vector.
                double assignment = this->node_to_data[node_label](
                    data_point_index, node->get_time_step());
                node->set_assignment(assignment);
            }
        }

        std::unique_ptr<Sampler> AncestralSampler::clone() {
            return std::make_unique<AncestralSampler>(*this);
        }

        //----------------------------------------------------------------------
        // Remove definitions
        //----------------------------------------------------------------------
#undef exists

    } // namespace model
} // namespace tomcat
