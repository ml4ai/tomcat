#include "Continuous.h"

#include "../pgm/RandomVariableNode.h"

namespace tomcat {
    namespace model {

        using namespace std;

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        Continuous::Continuous() {}

        Continuous::Continuous(vector<shared_ptr<Node>>& parameters)
            : parameters(parameters) {}

        Continuous::Continuous(vector<shared_ptr<Node>>&& parameters)
            : parameters(move(parameters)) {}

        Continuous::~Continuous() {}

        void Continuous::update_dependencies(Node::NodeMap& parameter_nodes_map,
                                             int time_step) {

            for (auto& parameter : this->parameters) {
                string parameter_timed_name;
                const NodeMetadata* metadata = parameter->get_metadata().get();
                if (metadata->is_replicable()) {
                    parameter_timed_name = metadata->get_timed_name(time_step);
                }
                else {
                    parameter_timed_name = metadata->get_timed_name(
                        metadata->get_initial_time_step());
                }

                if (parameter_nodes_map.count(parameter_timed_name) > 0) {
                    parameter = parameter_nodes_map[parameter_timed_name];
                }
            }
        }

        void Continuous::update_sufficient_statistics(
            const Eigen::VectorXd& sample) {
            for (auto& parameter : this->parameters) {
                if (parameter->get_metadata()->is_parameter()) {
                    if (RandomVariableNode* rv_node =
                            dynamic_cast<RandomVariableNode*>(
                                parameter.get())) {
                        rv_node->add_to_sufficient_statistics(sample);
                    }
                }
            }
        }

        Eigen::VectorXd Continuous::get_values() const {
            Eigen::VectorXd parameter_vector(this->parameters.size());

            int i = 0;
            for (const auto& parameter_node : this->parameters) {
                parameter_vector(i) =
                    parameter_node->get_assignment()(0, i);
            }

            return parameter_vector;
        }

    } // namespace model
} // namespace tomcat
