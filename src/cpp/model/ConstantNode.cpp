#include "ConstantNode.h"
#include <fmt/format.h>
#include <sstream>

namespace tomcat {
    namespace model {

        ConstantNode::ConstantNode(double value, std::string label) {
            this->assignment = Eigen::VectorXd(1);
            this->assignment(0) = value;
            this->create_default_metadata(label, 1);
        }

        void ConstantNode::create_default_metadata(std::string& label, int sample_size) {
            NodeMetadata metadata =
                NodeMetadata::create_single_time_link_metadata(label, 0, true, sample_size, 1);
            this->metadata = std::make_shared<NodeMetadata>(std::move(metadata));
        }

        void ConstantNode::print(std::ostream& os) const {
            os << this->get_description();
        }

        std::string ConstantNode::get_description() const {
            if (this->assignment.size() == 1) {
                std::stringstream assignment_string;
                assignment_string << this->assignment;

                return fmt::format("Constant({}, {})",
                                   this->metadata->get_label(),
                                   assignment_string.str());
            }
            else {
                std::stringstream assignment_string;
                assignment_string << this->assignment.transpose();

                return fmt::format("Constant({}, [{}])",
                                   this->metadata->get_label(),
                                   assignment_string.str());
            }
        }

        std::unique_ptr<Node> ConstantNode::clone() const {
            return std::make_unique<ConstantNode>(*this);
        }

        std::string ConstantNode::get_timed_name() const {
            return this->metadata->get_label();
        }

        std::string ConstantNode::get_timed_name(int time_step) const {
            return this->metadata->get_label();
        }
    } // namespace model
} // namespace tomcat