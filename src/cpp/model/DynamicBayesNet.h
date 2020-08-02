#pragma once

#include <boost/graph/adjacency_list.hpp>

#include "RandomVariableNode.h"

namespace tomcat {
    namespace model {

        //------------------------------------------------------------------
        // Structs
        //------------------------------------------------------------------
        struct VertexData {
            // This needs to be a shared pointer because some of the nodes can
            // be parameter nodes sharable among some CPDs
            std::shared_ptr<RandomVariableNode> node;
        };

        /**
         * Represents a Dynamic Bayes Net as a Directed Acyclic Graph. It is
         * comprised of node templates that are replicated into concrete timed
         * node instances when the DBN is unrolled. In this process, the nodes's
         * CPDs that depend on other nodes in the graph are updated to reference
         * the concrete instances of the nodes they depend on. By doing this,
         * it's guaranteed that a sample from these CPDs will condition on the
         * current assignments of the nodes in the DBN.
         */
        class DynamicBayesNet {
          public:
            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates an instance of a DBN.
             */
            DynamicBayesNet();

            /**
             * Creates an instance of a DBN and reserves space in the vector of
             * node templates.
             */
            DynamicBayesNet(int num_node_templates);

            ~DynamicBayesNet();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------

            // TODO - reevaluate if the default copy is enough. I guess that
            //  sampling from a DBN is changing the assignments of the nodes in
            //  the original DBN passed to the sampler.
            DynamicBayesNet(const DynamicBayesNet&) = default;

            DynamicBayesNet& operator=(const DynamicBayesNet&) = default;

            DynamicBayesNet(DynamicBayesNet&&) = default;

            DynamicBayesNet& operator=(DynamicBayesNet&&) = default;

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Adds node to the DBN as a template.
             *
             * @param node: node to be stored in the DBN as a template
             */
            void add_node_template(RandomVariableNode& node);

            /**
             * Adds node to the DBN as a template.
             *
             * @param node: node to be stored in the DBN as a template
             */
            void add_node_template(RandomVariableNode&& node);

            /**
             * Unrolls the DBN into time steps if nor previously unrolled into
             * the same number of time steps. This process creates vertices and
             * edges in the underlying graph structure by replicating nodes over
             * time, storing them into vertices linked to each other according
             * to the definitions in the nodes' metadata. It also updates the
             * CPDs that depend on other nodes with the concrete timed instances
             * of such nodes.
             *
             * @param time_steps: number of time steps to unroll into
             * @param force: whether the DBN should be forced to unroll again
             * even if it was previously unrolled over the same number of time
             * steps. This is useful if the DBN needs to be unrolled in the same
             * number of time steps as before but changed somehow (e.g. more
             * nodes were added to it).
             */
            void unroll(int time_steps, bool force);

            /**
             * Checks if the DBN is consistent and prepared to be unrolled.
             */
            void check();

            /**
             * Returns timed node objects in topological order.
             *
             * @return Times node objects in topological order.
             */
            std::vector<std::shared_ptr<RandomVariableNode>>
            get_nodes_topological_order() const;

            /**
             * Returns timed instances of the parents of a node
             * @param node: timed instance of a node
             * @param exclude_parameters: whether parameter nodes should be
             * excluded from the list of parent nodes
             * @return Time instances of a node's parents.
             */
            std::vector<std::shared_ptr<RandomVariableNode>>
            get_parent_nodes_of(const RandomVariableNode& node,
                                bool exclude_parameters) const;

            /**
             * Saves model's parameter values in individual files inside a given
             * folder.
             *
             * @param output_folder: folder where the files must be saved
             */
            void save_to_folder(const std::string& output_folder) const;

            /**
             * Loads model's parameter assignments from files previously saved
             * in a specific folder. The actual parameter nodes are excluded
             * from the model and the CPD's that depend on them are updated
             * with constant nodes containing values determined by the content
             * of the files processed.
             *
             * @param input_folder: folder where the files with the parameters'
             * values are saved
             */
            void load_from_folder(const std::string& input_folder);

            // --------------------------------------------------------
            // Getters & Setters
            // --------------------------------------------------------
            const std::vector<RandomVariableNode>& get_node_templates() const;

          private:
            //------------------------------------------------------------------
            // Types, Enums & Constants
            //------------------------------------------------------------------

            // The graph is defined as bidirectional to speed up the access to
            // the list of parents and children of a vertex. However, only
            // single-direction edges will be created in reality.
            typedef boost::adjacency_list<boost::vecS,
                                          boost::vecS,
                                          boost::bidirectionalS,
                                          VertexData>
                Graph;

            typedef std::unordered_map<std::string, int> IDMap;

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Creates vertices from a list of node templates.
             */
            void create_vertices_from_nodes();

            /**
             * Creates a vertex in the graph and stores a node timed instance in
             * it.
             *
             * @param node_template: node template
             * @param time_step
             * @return
             */
            VertexData add_vertex(const RandomVariableNode& node_template,
                                  int time_step);

            /**
             * Uses the node templates' metadata to link the vertices
             * accordingly.
             */
            void create_edges();

            /**
             * Adds a new edge to the graph.
             *
             * @param source_node: node where the edge should start from
             * @param target_node: node where the edge should end in
             * @param time_crossing: whether the edge should cross between one
             * time step to the subsequent one
             * @param target_time_step: time step of the timed instance of the
             * target node template
             */
            void add_edge(const NodeMetadata& source_node_metadata,
                          const NodeMetadata& target_node_metadata,
                          bool time_crossing,
                          int target_time_step);

            /**
             * Replaces node objects in the CPDs that depend on other nodes with
             * their concrete timed instance replica in the unrolled DBN.
             */
            void update_cpds();

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------
            Graph graph;
            IDMap name_to_id;

            // Mapping between a timed instance parameter node's label and its
            // node object.
            Node::NodeMap parameter_nodes_map;

            // Node templates will be used to create concrete instances of
            // nodes over time (timed node instances/objects), which will be
            // stored in the vertices of the unrolled DBN.
            //
            // The original list is preserved to allow multiple calls of the
            // unrolled method based on the original set of nodes.
            // TODO - change to a set to forbid adding the same node multiple
            //  times
            std::vector<RandomVariableNode> node_templates;

            // If unrolled, the number of time steps the DBN was unrolled into
            int time_steps;
        };

    } // namespace model
} // namespace tomcat