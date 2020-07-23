#include "DynamicBayesNet.h"
#include "CPD.h"
#include <fmt/core.h>

namespace tomcat {
    namespace model {
        DynamicBayesNet::DynamicBayesNet(int num_data_nodes,
                                         int num_parameter_nodes) {
            this->data_node_templates.reserve(num_data_nodes);
            this->parameter_node_templates.reserve(num_parameter_nodes);
        }

        void
        DynamicBayesNet::add_node(std::unique_ptr<RandomVariableNode>& node) {
            if (node->get_metadata()->parameter) {
                this->parameter_node_templates.push_back(
                    std::make_unique<RandomVariableNode>(*node));
            }
            else {
                this->data_node_templates.push_back(
                    std::make_unique<RandomVariableNode>(*node));
            }
        }

        void
        DynamicBayesNet::add_node(std::unique_ptr<RandomVariableNode>&& node) {
            if (node->get_metadata()->parameter) {
                this->parameter_node_templates.push_back(std::move(node));
            }
            else {
                this->data_node_templates.push_back(std::move(node));
            }
        }

        void DynamicBayesNet::unroll(int time_steps) {
            // todo - the way I am doing does not work for parent nodes that
            //  depend on other parent nodes.
            //  Fix by doing:
            //  1. Add vertices
            //  2. Add edges
            //  3. Update CPDs - process in topological order

            NodeMap parameter_nodes_map =
                this->create_vertices_from_parameter_nodes(time_steps);
            this->create_vertices_from_data_nodes(time_steps,
                                                  parameter_nodes_map);
        }

        DynamicBayesNet::NodeMap
        DynamicBayesNet::create_vertices_from_parameter_nodes(int time_steps) {

            NodeMap parameter_nodes_map;

            for (const auto& node : this->parameter_node_templates) {
                const std::shared_ptr<NodeMetadata> metadata =
                    node->get_metadata();
                if (metadata->repeatable) {
                    for (int t = metadata->initial_time_step; t < time_steps;
                         t++) {

                        // Create one instance of the parameter node as a vertex
                        // per time
                        VertexData vertex_data = this->add_vertex(node, t);
                        parameter_nodes_map[vertex_data.node
                                                ->get_timed_name()] =
                            vertex_data.node;
                    }
                }
                else {
                    // Create a single instance of the parameter node as a
                    // vertex which will be connected to all of the timed
                    // instances of a repeatable child data node over time.
                    VertexData vertex_data =
                        this->add_vertex(node, metadata->initial_time_step);
                    parameter_nodes_map[vertex_data.node->get_timed_name()] =
                        vertex_data.node;
                }
            }

            return parameter_nodes_map;
        }

        void DynamicBayesNet::create_vertices_from_data_nodes(
            int time_steps, NodeMap& parameter_nodes_map) {

            // List of CPDs already updated with concrete instances of the
            // parameter nodes in case they are node dependent. When a new copy
            // of a repeatable data node is created, if all of its parent
            // parameter nodes are not repeatable, the same CPD will be shared
            // among all of the replicas of the node.
            CPDMap updated_cpds;

            for (const auto& node : this->data_node_templates) {
                const std::shared_ptr<NodeMetadata> metadata =
                    node->get_metadata();
                if (metadata->repeatable) {
                    for (int t = metadata->initial_time_step; t < time_steps;
                         t++) {
                        VertexData vertex_data = this->add_vertex(node, t);

                        if (metadata->any_parameter_parents_repeatable) {
                            // Since at least one of the parameter parents
                            // is repeatable, that means that there are
                            // multiple instances of these repeatable
                            // parameter nodes over time and the data nodes
                            // that have CPD dependent of those parameters
                            // will need to reference the parameter instance
                            // corresponding to the node's time step,
                            // therefore, the CPDs cannot be shared.
                            std::shared_ptr<CPD> new_cpd =
                                node->get_cpd()->clone();
                            new_cpd->update_dependencies(parameter_nodes_map, t);
                            vertex_data.node->set_cpd(std::move(new_cpd));
                        }
                        else {
                            // The same set of parameter connects to all of
                            // the instances of this node over time, thus,
                            // its CPD only needs to be updated once.
                            if (updated_cpds.count(metadata->label) > 0) {
                                std::shared_ptr<CPD> new_cpd =
                                    updated_cpds[metadata->label];
                                vertex_data.node->set_cpd(std::move(new_cpd));
                            }
                            else {
                                std::shared_ptr<CPD> new_cpd =
                                    node->get_cpd()->clone();
                                new_cpd->update_dependencies(
                                    parameter_nodes_map, t);
                                updated_cpds[metadata->label] = new_cpd;
                                vertex_data.node->set_cpd(std::move(new_cpd));
                            }
                        }
                    }
                }
                else {
                    VertexData vertex_data =
                        this->add_vertex(node, metadata->initial_time_step);
                    if (!metadata->parameter) {
                        std::shared_ptr<CPD> new_cpd = node->get_cpd()->clone();
                        new_cpd->update_dependencies(parameter_nodes_map, metadata->initial_time_step);
                        vertex_data.node->set_cpd(std::move(new_cpd));
                    }
                }
            }
        }

        VertexData DynamicBayesNet::add_vertex(
            const std::unique_ptr<RandomVariableNode>& node, int time_step) {
            int vertex_id = boost::add_vertex(this->graph);

            VertexData data;
            // todo - check if the cpd and metadata are still shared
            data.node = std::make_shared<RandomVariableNode>(*node);
            data.node->set_time_step(time_step);
            this->graph[vertex_id] = std::move(data);

            this->name_to_id.insert(std::pair<std::string, int>(
                data.node->get_timed_name(), vertex_id));

            return data;
        }

        void DynamicBayesNet::add_edges() {
            // todo
        }

    } // namespace model
} // namespace tomcat