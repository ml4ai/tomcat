#include "Node.h"
#include <fmt/format.h>
#include <sstream>

namespace tomcat {
    namespace model {

        Node::Node(double value) {
            this->assignment = Eigen::VectorXd(1);
            this->assignment(0) = value;
        }

        void Node::print(std::ostream& os) const {
            os << this->get_description();
        }

        std::string Node::get_description() const {
            if (this->assignment.size() == 1) {
                std::stringstream assignment_string;
                assignment_string << this->assignment;

                return fmt::format("Node({})", assignment_string.str());
            }
            else {
                std::stringstream assignment_string;
                assignment_string << this->assignment.transpose();

                return fmt::format("Node([{}])", assignment_string.str());
            }
        }

        std::unique_ptr<Node> Node::clone() const {
            return std::make_unique<Node>(*this);
        }

        std::string Node::get_timed_name() const {
            return this->get_description();
        }
    } // namespace model
} // namespace tomcat