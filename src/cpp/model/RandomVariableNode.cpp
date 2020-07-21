#include "RandomVariableNode.h"
#include <stdexcept>

namespace tomcat {
    namespace model {

        void RandomVariableNode::print(std::ostream& os) const {
            if (this->assignment.size() == 0) {
                os << "RV(" << this->metadata->label;
                os << ", ";
                os << this->time_step;
                os << ", ";
                os << this->assignment;
                os << ")";
            }
            else {
                os << "RV(" << this->metadata->label;
                os << ", ";
                os << this->time_step;
                os << ", [";
                os << this->assignment.transpose();
                os << "])";
            }
        }

        void RandomVariableNode::set_assignment(double assignment) {
            if (this->assignment.size() == 0) {
                this->assignment(0) = assignment;
            }
            else {
                throw std::invalid_argument("The RV is not 1-dimensional.");
            }
        }

        std::unique_ptr<Node> RandomVariableNode::clone() const {
            return std::make_unique<RandomVariableNode>(*this);
        }

    } // namespace model
} // namespace tomcat