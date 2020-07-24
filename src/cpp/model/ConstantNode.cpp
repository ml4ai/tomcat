#include "ConstantNode.h"
#include <fmt/format.h>
#include <sstream>

namespace tomcat {
    namespace model {

        ConstantNode::ConstantNode(double value, std::string label) {
            this->assignment = Eigen::VectorXd(1);
            this->assignment(0) = value;
            this->create_default_metadata(label);
        }

        void ConstantNode::create_default_metadata(std::string& label) {
            this->metadata = std::make_shared<NodeMetadata>(NodeMetadata());
            this->metadata->label = label;
            this->metadata->repeatable = false;
            this->metadata->parameter = true;
        }

        void ConstantNode::print(std::ostream& os) const {
            os << this->get_description();
        }

        std::string ConstantNode::get_description() const {
            if (this->assignment.size() == 1) {
                std::stringstream assignment_string;
                assignment_string << this->assignment;

                return fmt::format("Constant({}, {})", this->metadata->label, assignment_string.str());
            }
            else {
                std::stringstream assignment_string;
                assignment_string << this->assignment.transpose();

                return fmt::format("Constant({}, [{}])", this->metadata->label, assignment_string.str());
            }
        }

        std::unique_ptr<Node> ConstantNode::clone() const {
            return std::make_unique<ConstantNode>(*this);
        }

        std::string ConstantNode::get_timed_name() const {
            return this->metadata->label;
        }

        std::string ConstantNode::get_timed_name(int time_step) const {
            return this->metadata->label;
        }
    } // namespace model
} // namespace tomcat