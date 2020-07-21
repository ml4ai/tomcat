#pragma once

#include "Node.h"
#include <boost/graph/directed_graph.hpp>
#include <iostream>
#include <memory>
#include <string>
#include <unordered_map>
#include <unordered_set>

namespace tomcat {
    namespace model {
        class DynamicBayesNet {
            typedef boost::
                adjacency_list<boost::vecS, boost::vecS, boost::directedS>
                    Graph;
            typedef boost::graph_traits<Graph>::vertex_descriptor Vertex;

          private:
            Graph graph;
            //std::unordered_map<std::string, int> name_to_id;
            std::vector<Node> nodes;

          public:
            DynamicBayesNet() {
                //
                //                Vertex v0 = this->graph.
                //
                //                Vertex v1 = this->graph.add_vertex();
                //                this->graph.add_edge(v0, v1);
            }
            ~DynamicBayesNet() {}

            void add_node(Node node) {
                this->nodes.push_back(std::move(node));
            }

            void unroll() {
                for (auto& node : this->nodes) {
                    std::cout << node << std::endl;
                }
            }
        };

    } // namespace model
} // namespace tomcat