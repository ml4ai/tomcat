#include "NodeMetadata.h"
#include "RandomVariableNode.h"
#include <fmt/format.h>

namespace tomcat {
    namespace model {

        using namespace std;
        
        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        NodeMetadata::NodeMetadata() {}

        NodeMetadata::NodeMetadata(string label,
                                   bool replicable,
                                   bool parameter,
                                   bool single_time_link,
                                   bool in_plate,
                                   int initial_time_step,
                                   int sample_size,
                                   int cardinality)
            : label(label), replicable(replicable), parameter(parameter),
              single_time_link(single_time_link),
              in_plate(in_plate), initial_time_step(initial_time_step),
              sample_size(sample_size), cardinality(cardinality) {}

        //----------------------------------------------------------------------
        // Destructor
        //----------------------------------------------------------------------
        NodeMetadata::~NodeMetadata() {}

        //----------------------------------------------------------------------
        // Operator overload
        //----------------------------------------------------------------------
        ostream& operator<<(ostream& os,
                                 const NodeMetadata& metadata) {

            os << metadata.get_description();

            return os;
        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        string NodeMetadata::get_description() const {
            stringstream ss;

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

        void
        NodeMetadata::add_parent_link(shared_ptr<NodeMetadata> parent_node,
                                      bool time_crossing) {
            // TODO - 1. error if parent node is a parameter node and time
            //  crossing
            //  is true. 2. Parameter cannot be replicable if child nodes are
            //  not
            //  Parent node cannot have continuous distribution and not be a
            //  parameter node

            if (!this->replicable && !this->single_time_link &&
                !time_crossing) {
                // todo - throw a warning exception because non-replicable,
                //  multi-time nodes always cross time.
            }

            this->parameter_parents |= parent_node->parameter;

            this->replicable_parameter_parent |=
                parent_node->parameter && parent_node->replicable;

            ParentLink link{parent_node, time_crossing};
            this->parent_links.push_back(link);
        }

        string NodeMetadata::get_timed_name(int time_step) const {
            return fmt::format("({},{})", this->label, time_step);
        }

        //----------------------------------------------------------------------
        // Getters & Setters
        //----------------------------------------------------------------------
        const string& NodeMetadata::get_label() const { return label; }

        int NodeMetadata::get_initial_time_step() const {
            return initial_time_step;
        }

        bool NodeMetadata::is_replicable() const { return replicable; }

        bool NodeMetadata::is_parameter() const { return parameter; }

        bool NodeMetadata::is_single_time_link() const {
            return single_time_link;
        }

        bool NodeMetadata::is_in_plate() const { return in_plate; }

        int NodeMetadata::get_sample_size() const { return sample_size; }

        int NodeMetadata::get_cardinality() const { return cardinality; }

        const vector<ParentLink>& NodeMetadata::get_parent_links() const {
            return parent_links;
        }

        bool NodeMetadata::has_parameter_parents() const {
            return parameter_parents;
        }

        bool NodeMetadata::has_replicable_parameter_parent() const {
            return replicable_parameter_parent;
        }

    } // namespace model
} // namespace tomcat