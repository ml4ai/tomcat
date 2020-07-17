#pragma once

#include <iostream>
#include <string>
#include <vector>

namespace tomcat {
    namespace model {

        // Defined in the end of this file.
        struct ParentLink;

        /* A node metadata contains information about the representation of a
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
            // repeatable node has only one copy of itself in an unrolled DBN.
            // This single instance is linked to all timed instances of its
            // dependents.
            bool repeatable;

            // List of parents of the node and their relative time step.
            std::vector<ParentLink> parent_links;

            void add_parent_link(const NodeMetadata& parent_node_metadata,
                                 bool time_crossing);

            friend std::ostream& operator<<(std::ostream& os,
                                            const NodeMetadata& metadata);
        };

        /* This struct represents a link between a child and a parent node. This
         * link is an abstraction as it identifies a parent node only by its
         * label but it will be transformed in a concrete edge in the
         * unrolled DBN.
         */
        struct ParentLink {

            NodeMetadata parent_node_metadata;

            // Edges crossing time indicate a dependency with the parent node
            // from a previous time step from where a concrete timed instance of
            // the child node is defined
            bool time_crossing;
        };

    } // namespace model
} // namespace tomcat
