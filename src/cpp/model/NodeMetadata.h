#pragma once

#include <iostream>
#include <memory>
#include <string>
#include <vector>

namespace tomcat {
    namespace model {

        // Defined in the end of this file.
        struct ParentLink;

        // Defined in a proper file; Cyclic reference.
        class RandomVariableNode;

        /** A node metadata contains information about the representation of a
         * node in a Dynamic Bayes Net (DBN) and its connections in the unrolled
         * version of such DBN over time.
         */
        struct NodeMetadata {
            // Unique identifier of a node in a DBN.
            std::string label;

            // Number of possible assignments if the node is sampled from a
            // discrete distribution and 0 otherwise.
            int cardinality;

            // The first time step the node shows up in an unrolled DBN.
            int initial_time_step;

            // If a node is repeatable, there will be multiple instances of it,
            // one for each time step from initial_time_step on, and each one of
            // these replicas will be linked to the corresponding replicas of
            // the dependencies of this node over time. On the other hand, a
            // non repeatable node has only one copy of itself in an unrolled
            // DBN. This single instance is linked to all timed instances of its
            // dependents.
            bool repeatable;

            // Whether a sample of the node is used as parameters of other
            // nodes's distribution
            bool parameter;

            // List of parents of the node and their relative time step.
            std::vector<ParentLink> parent_links;

            // Number of parent nodes that are parameter nodes. This variable is
            // kept to improve efficiency whenever the list of parameters
            // parents of a node is requested.
            int num_parameter_parents;

            // Indicates whether any of the parents that are parameter nodes
            // are repeatable. Storing this variable as parent links are added
            // will improve performance as this information need to be accessed
            // when the DBN is being unrolled.
            bool any_parameter_parents_repeatable;

            NodeMetadata() {
                this->any_parameter_parents_repeatable = false;
                this->num_parameter_parents = 0;
            }
            NodeMetadata(int num_parents) {
                this->any_parameter_parents_repeatable = false;
                this->num_parameter_parents = 0;
                this->parent_links.reserve(num_parents);
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
