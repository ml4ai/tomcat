#include "ConstantNode.h"

#include <fmt/format.h>

namespace tomcat {
    namespace model {
        //----------------------------------------------------------------------
        // Definitions
        //----------------------------------------------------------------------

        // No definitions in this file

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        ConstantNode::ConstantNode(double value, std::string label) {
            this->assignment = Eigen::VectorXd(1);
            this->assignment(0) = value;
            this->create_default_metadata(label, 1);
        }

        ConstantNode::ConstantNode(Eigen::VectorXd& values, std::string label) {
            this->assignment = values;
            this->create_default_metadata(label, this->assignment.size());
        }

        ConstantNode::ConstantNode(Eigen::VectorXd&& values,
                                   std::string label) {
            this->assignment = std::move(values);
            this->create_default_metadata(label, this->assignment.size());
        }

        ConstantNode::~ConstantNode() {}

        //----------------------------------------------------------------------
        // Copy & Move constructors/assignments
        //----------------------------------------------------------------------
        ConstantNode::ConstantNode(const ConstantNode& node) {
            this->metadata = node.metadata;
            this->assignment = node.assignment;
        }

        ConstantNode& ConstantNode::operator=(const ConstantNode& node) {
            this->metadata = node.metadata;
            this->assignment = node.assignment;
            return *this;
        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void ConstantNode::create_default_metadata(std::string& label,
                                                   int sample_size) {
            NodeMetadata metadata =
                NodeMetadata::create_single_time_link_metadata(
                    label, 0, true, sample_size, 1);
            this->metadata =
                std::make_shared<NodeMetadata>(std::move(metadata));
        }

        std::unique_ptr<Node> ConstantNode::clone() const {
            std::unique_ptr<ConstantNode> new_node =
                std::make_unique<ConstantNode>(*this);
            new_node->metadata =
                std::make_shared<NodeMetadata>(*this->metadata);
            return new_node;
        }

        std::string ConstantNode::get_timed_name() const {
            return this->metadata->get_label();
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

    } // namespace model
} // namespace tomcat