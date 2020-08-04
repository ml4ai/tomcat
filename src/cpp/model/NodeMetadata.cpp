#include "NodeMetadata.h"
#include "RandomVariableNode.h"
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
        NodeMetadata::NodeMetadata() {}

        NodeMetadata::NodeMetadata(std::string label,
                                   int initial_time_step,
                                   bool replicable,
                                   bool parameter,
                                   bool single_time_link,
                                   int sample_size,
                                   int cardinality)
            : label(label), initial_time_step(initial_time_step),
              replicable(replicable), parameter(parameter),
              single_time_link(single_time_link), sample_size(sample_size),
              cardinality(cardinality) {}

        //----------------------------------------------------------------------
        // Destructor
        //----------------------------------------------------------------------
        NodeMetadata::~NodeMetadata() {}

        //----------------------------------------------------------------------
        // Operator overload
        //----------------------------------------------------------------------
        std::ostream& operator<<(std::ostream& os,
                                 const NodeMetadata& metadata) {

            os << metadata.get_description();

            return os;
        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        std::string NodeMetadata::get_description() const {
            std::stringstream ss;

            ss << "Metadata: {\n";
            ss << " Label: " << this->label << "\n";
            ss << " Cardinality: " << this->cardinality << "\n";
            ss << " Initial Time Step: " << this->initial_time_step << "\n";
            ss << " Repeatable: " << this->replicable << "\n";
            if (!this->parent_links.empty()) {
                ss << " Parent Links:\n";
                ss << " [\n";

                for (auto& link : this->parent_links) {
                    ss << "  (";
                    ss << link.parent_node_metadata->label;
                    ss << ", ";
                    ss << link.time_crossing;
                    ss << ")\n";
                }
                ss << " ]\n";
            }

            ss << "}";

            return ss.str();
        }

        void NodeMetadata::add_parent_link(
            std::shared_ptr<NodeMetadata> parent_node,
            bool time_crossing) {
            // TODO - 1. error if parent node is a parameter node and time
            //  crossing
            //  is true. 2. Parameter cannot be replicable if child nodes are
            //  not

            if (!this->replicable && !this->single_time_link &&
                !time_crossing) {
                // todo - throw a warning exception because non-replicable,
                //  multi-time nodes always cross time.
            }

            this->parameter_parents |= parent_node->parameter;

            this->replicable_parameter_parent |=
                parent_node->parameter &&
                parent_node->replicable;

            ParentLink link{parent_node, time_crossing};
            this->parent_links.push_back(link);
        }

        std::string NodeMetadata::get_timed_name(int time_step) const {
            return fmt::format("({},{})", this->label, time_step);
        }

        //----------------------------------------------------------------------
        // Getters & Setters
        //----------------------------------------------------------------------
        const std::string& NodeMetadata::get_label() const { return label; }

        int NodeMetadata::get_initial_time_step() const {
            return initial_time_step;
        }

        bool NodeMetadata::is_replicable() const { return replicable; }

        bool NodeMetadata::is_parameter() const { return parameter; }

        bool NodeMetadata::is_single_time_link() const {
            return single_time_link;
        }

        int NodeMetadata::get_cardinality() const { return cardinality; }

        const std::vector<ParentLink>& NodeMetadata::get_parent_links() const {
            return parent_links;
        }

        bool NodeMetadata::has_parameter_parents() const {
            return parameter_parents;
        }

        bool NodeMetadata::has_replicable_parameter_parent() const {
            return replicable_parameter_parent;
        }

        int NodeMetadata::get_sample_size() const { return sample_size; }

    } // namespace model
} // namespace tomcat