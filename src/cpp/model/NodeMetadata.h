#pragma once

#include <iostream>
#include <memory>
#include <string>
#include <vector>

namespace tomcat {
    namespace model {

        // todo - I need an attribute to represent a parameter that only shows
        //  up in one time step: a prior

        // Defined in the end of this file.
        struct ParentLink;

        // Defined in a proper file; Cyclic reference.
        class RandomVariableNode;

        /** A node metadata contains information about the representation of a
         * node in a Dynamic Bayes Net (DBN) and its connections in the unrolled
         * version of such DBN over time.
         */
        class NodeMetadata {
          private:
            // Unique identifier of a node in a DBN.
            std::string label;

            // The first time step the node shows up in an unrolled DBN.
            int initial_time_step;

            // If a node is replicable, there will be multiple instances of it,
            // one for each time step from initial_time_step on, and each one of
            // these replicas will be linked to the corresponding replicas of
            // the dependencies of this node over time. On the other hand, a
            // non replicable node has only one copy of itself in an unrolled
            // DBN. This single instance is linked to all timed instances of its
            // dependents.
            bool replicable;

            // Indicates whether a sample of the node is used as parameters of
            // other nodes's distribution
            bool parameter;

            // Indicates whether the node is shows up and connects to its
            // children in only one time step of the unrolled DBN.
            bool single_time_link;

            // Number of possible assignments if the node is sampled from a
            // discrete distribution and 0 otherwise.
            // todo - use this to check whether the cpd is valid when adding it
            //  to a node
            int cardinality;

            // List of parents of the node and their relative time step.
            std::vector<ParentLink> parent_links;
            // Indicates whether the node that owns this metadata has any
            // parameter node as a parent. This variable is determined by the
            // parent links added to the metadata.
            bool parameter_parents = false;

            // Indicates whether the node has at least one parameter parent that
            // is replicable. This variable is determined by the parent links
            // added to the metadata.
            bool replicable_parameter_parent = false;

            // The constructor is private because some combinations of
            // parameters don't make sense and creating two constructors to deal
            // with this wouldn't be possible as they would have the same
            // signature. To avoid exposing a single constructor with all the
            // parameters and performing checks in it, and to make this class
            // invariant, proper public static methods are provided to
            // create valid instances.
            NodeMetadata(std::string label,
                         int initial_time_step,
                         bool replicable,
                         bool parameter,
                         bool single_time_link)
                : label(label), initial_time_step(initial_time_step),
                  replicable(replicable), parameter(parameter),
                  single_time_link(single_time_link) {}

          public:
            static NodeMetadata
            create_multiple_time_link_metadata(std::string label,
                                               int initial_time_step,
                                               bool replicable,
                                               bool parameter) {
                return NodeMetadata(
                    label, initial_time_step, replicable, parameter, false);
            }

            static NodeMetadata create_single_time_link_metadata(
                std::string label, int time_step, bool parameter) {
                return NodeMetadata(label, time_step, false, parameter, true);
            }

            /**
             * Add a new parent link to the list of parent links of the node
             * defined by the metadata
             *
             * @param parent_node_metadata: metadata of the parent node
             * @param time_crossing - flag indicating whether the parent node is
             *  in the previous or in the same time step as the node defined by
             *  the metadata. If the parent node is a parameter node, the
             *  time_crossing value is irrelevant as parameter nodes are time
             *  agnostic (they don't have replicas over time steps).
             */
            void
            add_parent_link(std::shared_ptr<RandomVariableNode> parent_node,
                            bool time_crossing);

            friend std::ostream& operator<<(std::ostream& os,
                                            const NodeMetadata& metadata);

            // --------------------------------------------------------
            // Getters
            // --------------------------------------------------------
            const std::string& get_label() const { return label; }
            int get_initial_time_step() const { return initial_time_step; }
            bool is_replicable() const { return replicable; }
            bool is_parameter() const { return parameter; }
            bool is_single_time_link() const { return single_time_link; }
            int get_cardinality() const { return cardinality; }
            const std::vector<ParentLink>& get_parent_links() const {
                return parent_links;
            }
            bool has_parameter_parents() const { return parameter_parents; }
            bool has_replicable_parameter_parent() const {
                return replicable_parameter_parent;
            }
        };

        /** This struct represents how a node should be linked to a given parent
         * node. This link is an abstraction as it identifies a parent node only
         * by its metadata. This will be transformed in a concrete edge in the
         * unrolled DBN.
         */
        struct ParentLink {

            std::shared_ptr<RandomVariableNode> parent_node;
            // Edges crossing time indicate a dependency with the parent node
            // from a previous time step from where a concrete timed instance of
            // the child node is defined
            bool time_crossing;
        };

    } // namespace model
} // namespace tomcat
