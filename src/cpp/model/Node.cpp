#include "Node.h"

namespace tomcat {
    namespace model {

        Node::Node(double value) {
            this->assignment = Eigen::VectorXd(1);
            this->assignment(0) = value;
        }

        void Node::print(std::ostream& os) const {
            if (this->assignment.size() == 1) {
                os << "Node(" << this->assignment << ")";
            }
            else {
                os << "Node([" << this->assignment.transpose() << "])";
            }
        }
    } // namespace model
} // namespace tomcat