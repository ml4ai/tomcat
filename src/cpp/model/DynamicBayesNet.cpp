#include "DynamicBayesNet.h"
#include <fmt/core.h>
#include "CPD.h"

namespace tomcat {
    namespace model {
        DynamicBayesNet::DynamicBayesNet(int num_data_nodes,
                                         int num_parameter_nodes) {
            this->data_nodes.reserve(num_data_nodes);
            this->parameter_nodes.reserve(num_parameter_nodes);
        }

        void
        DynamicBayesNet::add_node(std::unique_ptr<RandomVariableNode>& node) {
            if (node->get_metadata()->parameter) {
                this->parameter_nodes.push_back(
                    std::make_unique<RandomVariableNode>(*node));
            }
            else {
                this->data_nodes.push_back(
                    std::make_unique<RandomVariableNode>(*node));
            }
        }

        void
        DynamicBayesNet::add_node(std::unique_ptr<RandomVariableNode>&& node) {
            if (node->get_metadata()->parameter) {
                this->parameter_nodes.push_back(std::move(node));
            }
            else {
                this->data_nodes.push_back(std::move(node));
            }
        }

        void DynamicBayesNet::unroll(int time_steps) {
            this->create_vertices_from_nodes(this->parameter_nodes, time_steps);
            this->create_vertices_from_nodes(this->data_nodes, time_steps);
        }

        void DynamicBayesNet::create_vertices_from_nodes(
            const std::vector<std::unique_ptr<RandomVariableNode>>& nodes,
            int time_steps) {
            for (const auto& node : nodes) {
                const std::shared_ptr<NodeMetadata> metadata =
                    node->get_metadata();
                if (metadata->repeatable) {
                    for (int t = metadata->initial_time_step; t < time_steps;
                         t++) {
                        VertexData vertex_data = this->add_vertex(node, t);
                        if (!metadata->parameter) {
                            // map of parameter name and node
                            // vertex_data.node->get_cpd()->update_dependencies(map);
                        }
                    }
                }
                else {
                    VertexData vertex_data = this->add_vertex(node, metadata->initial_time_step);
                    if (!metadata->parameter) {
                        // map of parameter name and node
                        // vertex_data.node->get_cpd()->update_dependencies(map);
                    }
                }
            }
        }

        VertexData DynamicBayesNet::add_vertex(
            const std::unique_ptr<RandomVariableNode>& node, int time_step) {
            int vertex_id = boost::add_vertex(this->graph);

            VertexData data;
            data.node = std::make_shared<RandomVariableNode>(*node);
            data.node->set_time_step(time_step);
            this->graph[vertex_id] = std::move(data);

            this->name_to_id.insert(std::pair<std::string, int>(
                data.node->get_timed_name(), vertex_id));

            return data;
        }

    } // namespace model
} // namespace tomcat