#include "RandomVariableNode.h"
#include <fmt/format.h>
#include <stdexcept>
#include <sstream>

namespace tomcat {
    namespace model {

        void RandomVariableNode::print(std::ostream& os) const {
            os << this->get_description();
        }

        std::string RandomVariableNode::get_description() const {


            if (this->assignment.size() == 1) {
                std::stringstream assignment_string;
                assignment_string << this->assignment;

                return fmt::format("RV({}, {}, {})",
                                   this->metadata->label,
                                   this->time_step,
                                   assignment_string.str());
            }
            else {
                std::stringstream assignment_string;
                assignment_string << this->assignment.transpose();

                return fmt::format("RV({}, {}, [{}])",
                                   this->metadata->label,
                                   this->time_step,
                                   assignment_string.str());
            }
        }

        void RandomVariableNode::set_assignment(double assignment) {
            if (this->assignment.size() == 1) {
                this->assignment(0) = assignment;
            }
            else {
                throw std::invalid_argument("The RV is not 1-dimensional.");
            }
        }

        std::unique_ptr<Node> RandomVariableNode::clone() const {
            return std::make_unique<RandomVariableNode>(*this);
        }

        std::vector<std::shared_ptr<RandomVariableNode>>
        RandomVariableNode::get_parameter_parents() const {
            std::vector<std::shared_ptr<RandomVariableNode>> parameter_parents;
            parameter_parents.reserve(this->metadata->num_parameter_parents);

            for (const auto& parent_link : this->metadata->parent_links) {
                if (parent_link.parent_node->metadata->parameter) {
                    parameter_parents.push_back(parent_link.parent_node);
                }
            }

            return parameter_parents;
        }

        std::string RandomVariableNode::get_timed_name() const {
            return fmt::format("({},{})", metadata->label, this->time_step);
        }

    } // namespace model
} // namespace tomcat