#include "Continuous.h"

#include "../pgm/RandomVariableNode.h"

namespace tomcat {
    namespace model {

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        Continuous::Continuous() {}

        Continuous::Continuous(std::vector<std::shared_ptr<Node>>& parameters)
            : parameters(parameters) {}

        Continuous::Continuous(std::vector<std::shared_ptr<Node>>&& parameters)
            : parameters(std::move(parameters)) {}

        Continuous::~Continuous() {}

        void Continuous::update_dependencies(Node::NodeMap& parameter_nodes_map,
                                             int time_step) {

            for (auto& parameter : this->parameters) {
                std::string parameter_timed_name;
                const NodeMetadata* metadata =
                    parameter->get_metadata().get();
                if (metadata->is_replicable()) {
                    parameter_timed_name = metadata->get_timed_name(time_step);
                }
                else {
                    parameter_timed_name = metadata->get_timed_name(
                        metadata->get_initial_time_step());
                }

                if (parameter_nodes_map.count(parameter_timed_name) > 0) {
                    parameter =
                        parameter_nodes_map[parameter_timed_name];
                }
            }
        }

        void Continuous::update_sufficient_statistics(const Eigen::VectorXd& sample) {
            for(auto& parameter : this->parameters){
                if (parameter->get_metadata()->is_parameter()) {
                    if (RandomVariableNode* rv_node =
                            dynamic_cast<RandomVariableNode*>(
                                parameter.get())) {
                        rv_node->add_to_sufficient_statistics(sample);
                    }
                }
            }
        }

    } // namespace model
} // namespace tomcat
