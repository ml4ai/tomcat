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
            typedef boost::adjacency_list<boost::vecS,
                                          boost::vecS,
                                          boost::directedS,
                                          VertexData>
                Graph;

          private:
            Graph graph;
            std::unordered_map<std::string, int> name_to_id;
            // The actual nodes which will be stored as vertex data will be
            // replicas of the nodes in this list. The original list is
            // preserved to allow multiple calls of the unrolled method based on
            // the original set of nodes.
            //
            // Parameter nodes are nodes which samples are used as parameters of
            // other nodes' distributions.
            //
            // Data nodes are the latent and
            // observable nodes in the DBN that are not parameter nodes.
            std::vector<std::unique_ptr<RandomVariableNode>> data_nodes;
            std::vector<std::unique_ptr<RandomVariableNode>> parameter_nodes;

            /**
             * Create vertices from a list of nodes.
             */
            void create_vertices_from_nodes(
                const std::vector<std::unique_ptr<RandomVariableNode>>& nodes,
                int time_steps);

            VertexData
            add_vertex(const std::unique_ptr<RandomVariableNode>& node,
                       int time_step);

            /**
             * Replace parameter nodes in a node dependent CPD by the correct
             * replica of the node in the unrolled DBN.
             *
             * @param vertex_data: vertex data with the node which CPD has to be
             * updated
             */
            void update_cpd_dependencies(VertexData& vertex_data);

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