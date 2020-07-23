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
            std::shared_ptr<RandomVariableNode> node;
        };

        class DynamicBayesNet {
          private:
            typedef boost::adjacency_list<boost::vecS,
                                          boost::vecS,
                                          boost::directedS,
                                          VertexData>
                Graph;

            typedef std::unordered_map<std::string, std::shared_ptr<Node>>
                NodeMap;
            typedef std::unordered_map<std::string, std::shared_ptr<CPD>>
                CPDMap;
            typedef std::unordered_map<std::string, int> IDMap;

            Graph graph;
            IDMap name_to_id;
            // The actual nodes which will be stored as vertex data will be
            // replicas of the nodes in this list. The original list is
            // preserved to allow multiple calls of the unrolled method based on
            // the original set of nodes.
            //
            // Templates of the parameter node are nodes which samples are used
            // as parameters of other nodes' distributions.
            //
            // Templates of data nodes are the latent and
            // observable nodes in the DBN that are not parameter nodes.
            //
            // Those templates will be used to create concrete instances of
            // nodes  over time, which will be stored in the vertices of the
            // unrolled graph.
            std::vector<std::unique_ptr<RandomVariableNode>>
                data_node_templates;
            std::vector<std::unique_ptr<RandomVariableNode>>
                parameter_node_templates;

            /**
             * Create vertices from a list of data nodes.
             *
             * @param time_steps: number of tim steps to unroll
             */
            NodeMap create_vertices_from_parameter_nodes(int time_steps);

            /**
             * Create vertices from a list of parameter nodes. This method
             * assumes create_vertices_from_parameter_nodes was called first as
             * the data nodes will need to update their CPD's with the parameter
             * nodes already created and stored in vertices of the graph.
             *
             * @param time_steps: number of tim steps to unroll
             */
            void create_vertices_from_data_nodes(int time_steps,
                                                 NodeMap& parameter_nodes_map);

            VertexData
            add_vertex(const std::unique_ptr<RandomVariableNode>& node,
                       int time_step);

            void add_edges();

          public:
            DynamicBayesNet() {}
            DynamicBayesNet(int num_data_nodes, int num_parameter_nodes);
            ~DynamicBayesNet() {}

            /**
             * Store node in the DBN as a copy of the argument
             *
             * @param node: node to be copied and stored in the DBN
             */
            void add_node(std::unique_ptr<RandomVariableNode>& node);

            /**
             * Move node's reference to the DBN
             *
             * @param node: node to be moved to the DBN
             */
            void add_node(std::unique_ptr<RandomVariableNode>&& node);

            /**
             * Create vertices and edges by replicating nodes over time and
             * linking them according to the list of original nodes and their
             * metadata.
             */
            void unroll(int time_steps);
        };

    } // namespace model
} // namespace tomcat