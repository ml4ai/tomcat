#include "DynamicBayesNet.h"
#include "CPD.h"
#include <fmt/core.h>

namespace tomcat {
    namespace model {

        DynamicBayesNet::NodeMap DynamicBayesNet::create_vertices_from_nodes() {

            NodeMap parameter_nodes_map;

            for (const auto& node : this->node_templates) {
                const std::shared_ptr<NodeMetadata> metadata =
                    node.get_metadata();
                if (metadata->repeatable) {
                    for (int t = metadata->initial_time_step; t < time_steps;
                         t++) {

                        VertexData vertex_data = this->add_vertex(node, t);
                        if (vertex_data.node->get_metadata()->parameter) {
                            parameter_nodes_map[vertex_data.node
                                                    ->get_timed_name()] =
                                vertex_data.node;
                        }
                    }
                }
                else {
                    VertexData vertex_data =
                        this->add_vertex(node, metadata->initial_time_step);
                    if (vertex_data.node->get_metadata()->parameter) {
                        parameter_nodes_map[vertex_data.node
                                                ->get_timed_name()] =
                            vertex_data.node;
                    }
                }
            }

            return parameter_nodes_map;
        }

        VertexData DynamicBayesNet::add_vertex(const RandomVariableNode& node,
                                               int time_step) {
            int vertex_id = boost::add_vertex(this->graph);

            VertexData data;
            // todo - check if the cpd and metadata are still shared
            data.node = std::make_shared<RandomVariableNode>(node);
            data.node->set_time_step(time_step);
            data.node->get_cpd()->reset_updated();
            this->graph[vertex_id] = std::move(data);

            this->name_to_id[data.node->get_timed_name()] = vertex_id;

            return data;
        }

        void DynamicBayesNet::create_edges() {
            for (const auto& node : this->node_templates) {
                const std::shared_ptr<NodeMetadata> metadata =
                    node.get_metadata();

                for (const auto& parent_link : metadata->get_parent_links()) {
                    if (metadata->repeatable) {
                        for (int t = metadata->initial_time_step;
                             t < time_steps;
                             t++) {

                            this->add_edge(*parent_link.parent_node, node, t);
                        }
                    }
                    else {
                        this->add_edge(*parent_link.parent_node,
                                       node,
                                       metadata->initial_time_step);
                    }
                }
            }
        }

        void DynamicBayesNet::add_edge(const RandomVariableNode& source_node,
                                       const RandomVariableNode& target_node,
                                       int target_time_step) {

            int parent_time_step = -1;
            if (source_node.get_metadata()->repeatable) {
                // Links with repeatable parameter nodes are time agnostic (they
                // happen between a parent node and a data node in the same time
                // step)
                if (source_node.get_metadata()->initial_time_step <=
                    target_time_step) {
                    parent_time_step = target_time_step;
                }
            }
            else {
                parent_time_step =
                    source_node.get_metadata()->initial_time_step;
            }
            if (parent_time_step >= 0) {
                int source_vertex_id =
                    this->name_to_id[source_node.get_timed_name(
                        parent_time_step)];
                int target_vertex_id =
                    this->name_to_id[target_node.get_timed_name(
                        target_time_step)];
                boost::add_edge(
                    source_vertex_id, target_vertex_id, this->graph);
            }
        }

        void DynamicBayesNet::update_cpds(NodeMap& parameter_nodes_map) {
            for (const auto& node : this->node_templates) {
                if (node.get_metadata()->has_parameter_parents()) {
                    if (!node.get_cpd()->is_updated()) {
                        node.get_cpd()->update_dependencies(
                            parameter_nodes_map, node.get_time_step());
                    }
                }
            }
        }

        void DynamicBayesNet::check() {
            // todo
        }

        DynamicBayesNet::DynamicBayesNet(int num_nodes) {
            this->node_templates.reserve(num_nodes);
        }

        void
        DynamicBayesNet::add_node(RandomVariableNode& node) {
            this->node_templates.push_back(node);
        }

        void
        DynamicBayesNet::add_node(RandomVariableNode&& node) {
            this->node_templates.push_back(std::move(node));
        }

        void DynamicBayesNet::unroll(int time_steps) {
            this->time_steps = time_steps;
            NodeMap parameter_nodes_map = this->create_vertices_from_nodes();
            this->create_edges();
            this->update_cpds(parameter_nodes_map);
        }

    } // namespace model
} // namespace tomcat