#include "DBNSamplingTrainer.h"

namespace tomcat {
    namespace model {

        //----------------------------------------------------------------------
        // Definitions
        //----------------------------------------------------------------------

#define exists(member, container) (container.find(member) != container.end())

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        DBNSamplingTrainer::DBNSamplingTrainer(std::shared_ptr<Sampler> sampler)
            : sampler(sampler) {}

        DBNSamplingTrainer::~DBNSamplingTrainer() {}

        //----------------------------------------------------------------------
        // Copy & Move constructors/assignments
        //----------------------------------------------------------------------

        //----------------------------------------------------------------------
        // Operator overload
        //----------------------------------------------------------------------

        //----------------------------------------------------------------------
        // Static functions
        //----------------------------------------------------------------------

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void DBNSamplingTrainer::fit(std::shared_ptr<gsl_rng> random_generator, int num_samples) {
            this->sampler->sample(random_generator, num_samples);

            std::shared_ptr<DynamicBayesNet> model = this->sampler->get_model();
            std::unordered_map<std::string, Tensor3> node_label_to_samples;

            for (const auto& node : model->get_parameter_nodes()) {
                std::shared_ptr<RandomVariableNode> rv_node =
                    std::dynamic_pointer_cast<RandomVariableNode>(node);
                if (!rv_node->is_frozen()) {
                    std::string node_label = node->get_metadata()->get_label();
                    if (!exists(node_label, node_label_to_samples)) {
                        node_label_to_samples[node_label] =
                            this->sampler->get_samples(node_label);
                    }

                    int time_step = rv_node->get_time_step();
                    Eigen::MatrixXd samples =
                        node_label_to_samples[node_label](time_step, 2)
                            .transpose();
                    Eigen::MatrixXd mean_value(1, samples.cols());
                    mean_value = samples.colwise().mean();
                    rv_node->set_assignment(mean_value);
                }
            }
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
