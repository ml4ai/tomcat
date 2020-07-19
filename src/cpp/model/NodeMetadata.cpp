#include "NodeMetadata.h"

namespace tomcat {
    namespace model {

        // Convenience method to insert a parent link
        void
        NodeMetadata::add_parent_link(const NodeMetadata& parent_node_metadata,
                                      bool time_crossing) {

            ParentLink link{parent_node_metadata, time_crossing};
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
                    os << link.parent_node_metadata.label;
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