#include "RandomVariableNode.h"
#include <fmt/format.h>
#include <stdexcept>
#include <sstream>

namespace tomcat {
    namespace model {

        void RandomVariableNode::copy_from_node(const RandomVariableNode &node)  {
            this->metadata = node.metadata;
            this->cpd = node.cpd;
            this->time_step = node.time_step;
            this->assignment = node.assignment;
        }

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

        std::string RandomVariableNode::get_timed_name() const {
            return this->get_timed_name(this->time_step);
        }

        std::string RandomVariableNode::get_timed_name(int time_step) const {
            return fmt::format("({},{})", metadata->label, time_step);
        }

    } // namespace model
} // namespace tomcat