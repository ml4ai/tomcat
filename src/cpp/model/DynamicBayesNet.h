#pragma once

#include "RandomVariableNode.h"
#include <boost/graph/adjacency_list.hpp>
#include <iostream>
#include <memory>
#include <string>
#include <unordered_map>
#include <unordered_set>

namespace tomcat {
    namespace model {

        struct VertexData {
            // This needs to be a shared pointer because some of the nodes can
            // be parameter nodes sharable among some CPDs
            std::shared_ptr<RandomVariableNode> node;
        };

        class DynamicBayesNet {
          private:
            // The graph is defined as bidirectional to improve accessing the
            // list of parents and children of a vertex, however, only
            // single-direction edges will be created in reality.
            typedef boost::adjacency_list<boost::vecS,
                                          boost::vecS,
                                          boost::bidirectionalS,
                                          VertexData>
                Graph;

            typedef std::unordered_map<std::string, std::shared_ptr<Node>>
                NodeMap;
            typedef std::unordered_map<std::string, std::shared_ptr<CPD>>
                CPDMap;
            typedef std::unordered_map<std::string, int> IDMap;

            Graph graph;
            IDMap name_to_id;

            // Node templates will be used to create concrete instances of
            // nodes over time, which will be stored in the vertices of the
            // unrolled DBN.
            //
            // The original list is preserved to allow multiple calls of the
            // unrolled method based on the original set of nodes.
            // todo - change to set to forbid adding the same node multiple
            // times
            std::vector<RandomVariableNode> node_templates;

            // If unrolled, the number of time steps the DBN was unrolled in
            int time_steps;

            /**
             * Create vertices from a list of nodes.
             *
             * @return Map between parameter nodes' name and content. This map
             * will be used for updating the node dependent CPDs with concrete
             * instances of the parameter nodes created in this method.
             */
            NodeMap create_vertices_from_nodes();

            // todo - document these methods
            VertexData add_vertex(const RandomVariableNode& node,
                                  int time_step);

            void create_edges();

            void add_edge(const RandomVariableNode& source_node,
                          const RandomVariableNode& target_node,
                          bool time_crossing,
                          int target_time_step);

            void update_cpds(NodeMap& parameter_nodes_map);

          public:
            DynamicBayesNet() {}
            DynamicBayesNet(int num_nodes);
            ~DynamicBayesNet() {}

            /**
             * Store copy of the node in the DBN
             *
             * @param node: node to be stored in the DBN
             */
            void add_node_template(RandomVariableNode& node);

            /**
             * Move the node to the DBN
             *
             * @param node: node to be stored in the DBN
             */
            void add_node_template(RandomVariableNode&& node);

            /**
             * Create vertices and edges by replicating nodes over time and
             * linking them according to the list of original nodes and their
             * metadata.
             *
             * @param time_steps: number of time steps to unroll
             * @param force: whether the DBN should be forced to unroll again
             * even if it was previously unrolled over the same number of time
             * steps. This is useful if more nodes were added to the DBN.
             */
            void unroll(int time_steps, bool force);

            void check();

            std::vector<std::shared_ptr<RandomVariableNode>>
            get_nodes_topological_order() const;

            std::vector<std::shared_ptr<RandomVariableNode>>
            get_parent_nodes_of(const RandomVariableNode& node,
                                bool exclude_parameters) const;

            // --------------------------------------------------------
            // Getters
            // --------------------------------------------------------
            const std::vector<RandomVariableNode>& get_nodes() const {
                return node_templates;
            }
        };

    } // namespace model
} // namespace tomcat