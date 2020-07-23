#include "NodeMetadata.h"
#include "RandomVariableNode.h"

namespace tomcat {
    namespace model {

        // Convenience method to insert a parent link
        void NodeMetadata::add_parent_link(
            std::shared_ptr<RandomVariableNode> parent_node,
            bool time_crossing) {

            if (parent_node->get_metadata()->parameter) {
                this->num_parameter_parents += 1;

                this->any_parameter_parents_repeatable =
                    this->any_parameter_parents_repeatable ||
                    parent_node->get_metadata()->repeatable;
            }

            ParentLink link{parent_node, time_crossing};
            this->parent_links.push_back(link);
        }

        std::ostream& operator<<(std::ostream& os,
                                 const NodeMetadata& metadata) {

            os << "Metadata: {\n";
            os << " Label: " << metadata.label << "\n";
            os << " Cardinality: " << metadata.cardinality << "\n";
            os << " Initial Time Step: " << metadata.initial_time_step << "\n";
            os << " Repeatable: " << metadata.repeatable << "\n";
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