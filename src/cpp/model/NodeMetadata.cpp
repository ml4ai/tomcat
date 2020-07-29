#include "NodeMetadata.h"
#include "RandomVariableNode.h"

namespace tomcat {
    namespace model {

        void NodeMetadata::add_parent_link(
            std::shared_ptr<RandomVariableNode> parent_node,
            bool time_crossing) {
            // todo - 1. error if parent node is a parameter node and time
            // crossing
            //  is true. 2. Parameter cannot be replicable if child nodes are
            //  not

            if (!this->replicable && !this->single_time_link &&
                !time_crossing) {
                // todo - throw a warning exception because non-replicable,
                //  multi-time nodes always cross time.
            }

            this->parameter_parents |= parent_node->get_metadata()->parameter;

            this->replicable_parameter_parent |=
                parent_node->get_metadata()->parameter &&
                parent_node->get_metadata()->replicable;

            ParentLink link{parent_node, time_crossing};
            this->parent_links.push_back(link);
        }

        std::ostream& operator<<(std::ostream& os,
                                 const NodeMetadata& metadata) {

            os << "Metadata: {\n";
            os << " Label: " << metadata.label << "\n";
            os << " Cardinality: " << metadata.cardinality << "\n";
            os << " Initial Time Step: " << metadata.initial_time_step << "\n";
            os << " Repeatable: " << metadata.replicable << "\n";
            if (!metadata.parent_links.empty()) {
                os << " Parent Links:\n";
                os << " [\n";

                for (auto& link : metadata.parent_links) {
                    os << "  (";
                    os << link.parent_node->get_metadata()->label;
                    os << ", ";
                    os << link.time_crossing;
                    os << ")\n";
                }
                os << " ]\n";
            }

            os << "}";

            return os;
        }

    } // namespace model
} // namespace tomcat