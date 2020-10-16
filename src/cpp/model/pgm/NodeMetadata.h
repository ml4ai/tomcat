#pragma once

#include <iostream>
#include <memory>
#include <string>
#include <vector>
#include <unordered_map>

#include "utils/Definitions.h"

namespace tomcat {
    namespace model {

        //------------------------------------------------------------------
        // Forward declarations
        //------------------------------------------------------------------

        class NodeMetadata;

        //------------------------------------------------------------------
        // Structs
        //------------------------------------------------------------------

        /** This struct represents how a node should be linked to a given parent
         * node. This link is an abstraction as it identifies a parent node only
         * by its metadata. This will be transformed in a concrete edge in the
         * unrolled DBN.
         */
        struct ParentLink {

            std::shared_ptr<NodeMetadata> parent_node_metadata;
            // Edges crossing time indicate a dependency with the parent node
            // from a previous time step from where a concrete timed instance of
            // the child node is defined
            bool time_crossing;
        };

        /**
         * A node metadata contains information about the representation of a
         * node and its connections in an unrolled DBN.
         */
        class NodeMetadata {
          public:
            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------
            /**
             * Empty constructor to allow usage in map structures.
             */
            NodeMetadata();

            /**
             * Creates an instance of NodeMetadata for a node that shows up or
             * has connections crossing multiple time steps.
             *
             * @param label: node's label
             * @param initial_time_step: first time step the node shows up in
             * the unrolled DBN
             * @param replicable: whether the node shows up in successive time
             * steps after its first appearance in the unrolled DBN
             * @param parameter: whether the node is a parameter node in which
             * its assignment determines other node's(s') distribution
             * parameters
             * @param sample_size: the dimensionality of a sample from the node
             * @param cardinality: the number of possible assignments the node
             * can take if it's sampled from a discrete distribution. Otherwise,
             * the cardinality should be set to 0.
             *
             * @return Valid instance of a metadata object for the node
             */
            static NodeMetadata
            create_multiple_time_link_metadata(std::string label,
                                               bool replicable,
                                               bool parameter,
                                               bool in_plate,
                                               int initial_time_step,
                                               int sample_size,
                                               int cardinality = 0) {
                return NodeMetadata(label,
                                    replicable,
                                    parameter,
                                    false,
                                    in_plate,
                                    initial_time_step,
                                    sample_size,
                                    cardinality);
            }

            /**
             * Create an instance of NodeMetadata for a node that shows up once
             * and has connections in a single time step only.
             *
             * @param label: node's label
             * @param time_step: unique time step the node shows up in
             * the unrolled DBN
             * @param parameter: whether the node is a parameter node in which
             * its assignment determines other node's(s') distribution
             * parameters
             * @param sample_size: the dimensionality of a sample from the node
             * @param cardinality: the number of possible assignments the node
             * can take if it's sampled from a discrete distribution. Otherwise,
             * the cardinality should be set to 0.
             *
             * @return Valid instance of a metadata object for the node
             */
            static NodeMetadata
            create_single_time_link_metadata(std::string label,
                                             bool parameter,
                                             bool in_plate,
                                             int time_step,
                                             int sample_size,
                                             int cardinality = 0) {
                return NodeMetadata(label,
                                    false,
                                    parameter,
                                    true,
                                    in_plate,
                                    time_step,
                                    sample_size,
                                    cardinality);
            }

            ~NodeMetadata();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------
            NodeMetadata(const NodeMetadata&) = default;

            NodeMetadata& operator=(const NodeMetadata&) = default;

            NodeMetadata(NodeMetadata&&) = default;

            NodeMetadata& operator=(NodeMetadata&&) = default;

            //------------------------------------------------------------------
            // Operator overload
            //------------------------------------------------------------------
            friend std::ostream& operator<<(std::ostream& os,
                                            const NodeMetadata& metadata);

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Adds a new parent link to the list of parent links of the node
             * defined by the metadata
             *
             * @param parent_node_metadata: metadata of the parent node
             * @param time_crossing - flag indicating whether the parent node is
             *  in the previous or in the same time step as the node defined by
             *  the metadata.
             */
            void add_parent_link(std::shared_ptr<NodeMetadata> parent_node,
                                 bool time_crossing);

            /**
             * Returns the node's unique id in an unrolled DBN for an arbitrary
             * time step.
             *
             * @return Node's description in an unrolled DBN for an arbitrary
             * time step.
             */
            std::string get_timed_name(int time_step) const;

            //------------------------------------------------------------------
            // Getters & Setters
            //------------------------------------------------------------------
            const std::string& get_label() const;

            bool is_replicable() const;

            bool is_parameter() const;

            bool is_single_time_link() const;

            bool is_in_plate() const;

            int get_initial_time_step() const;

            int get_sample_size() const;

            int get_cardinality() const;

            const std::vector<ParentLink>& get_parent_links() const;

            bool has_parameter_parents() const;

            bool has_replicable_parameter_parent() const;

          private:
            //------------------------------------------------------------------
            // Constructor
            //------------------------------------------------------------------

            /**
             * Creates an instance of a metadata for a node. This constructor is
             * private because some combinations of parameters don't make sense
             * and creating two constructors to deal with this wouldn't be
             * possible as they would have the same signature. To avoid exposing
             * a single constructor with all the parameters and performing
             * checks in it, and to make this class consistent, proper public
             * static methods are provided to create valid instances.
             */
            NodeMetadata(std::string label,
                         bool replicable,
                         bool parameter,
                         bool single_time_link,
                         bool in_plate,
                         int initial_time_step,
                         int sample_size,
                         int cardinality);

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Returns a short description of the metadata.
             *
             * @return Metadata's description.
             */
            std::string get_description() const;

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------

            // Unique identifier of a node in a DBN.
            std::string label;

            // If a node is replicable, there will be multiple instances of it,
            // one for each time step starting from initial_time_step, and each
            // one of these replicas will be linked to the corresponding
            // replicas of the dependencies of this node over time. On the other
            // hand, a not replicable node has only one copy of itself in an
            // unrolled DBN. This single instance is linked to all timed
            // instances of its dependents.
            bool replicable;

            // Indicates whether a sample of the node is used as parameters of
            // other nodes's distribution
            bool parameter;

            // Indicates whether the node shows up and connects to its
            // children in only one time step of the unrolled DBN.
            bool single_time_link;

            // Whether the node is inside a plate diagram when data is provided
            // which means that in a single time step there are as many
            // instances of the node as the number of data points.
            bool in_plate;

            // The first time step the node shows up in an unrolled DBN.
            int initial_time_step;

            // Dimensionality of a sample from the node.
            int sample_size;

            // Number of possible assignments if the node is sampled from a
            // discrete distribution and 0 otherwise.
            // TODO - use this to check whether the cpd is valid when adding it
            //  to a node
            int cardinality;

            // List of parents of the node and their relative time step.
            std::vector<ParentLink> parent_links;

            // Indicates whether the node that owns this metadata has any
            // parameter node as a parent. This variable is updated as
            // parent links are added to the metadata.
            bool parameter_parents = false;

            // Indicates whether the node has at least one parameter parent that
            // is replicable. This variable is updated as parent links are added
            // to the metadata. If all the parameter parents of a node are not
            // replicable, updating the CPD of one of the replicas of the node
            // is enough as that CPD is going to be shared among all of the
            // replicas of the node in the unrolled DBN. Therefore, this
            // information is stored to avoid unnecessary CPD updates when
            // unrolling a DBN.
            bool replicable_parameter_parent = false;

        };

    } // namespace model
} // namespace tomcat
