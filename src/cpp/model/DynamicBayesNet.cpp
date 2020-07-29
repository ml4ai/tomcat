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
                if (metadata->is_replicable()) {
                    for (int t = metadata->get_initial_time_step();
                         t < this->time_steps;
                         t++) {

                        VertexData vertex_data = this->add_vertex(node, t);
                        if (vertex_data.node->get_metadata()->is_parameter()) {
                            parameter_nodes_map[vertex_data.node
                                                    ->get_timed_name()] =
                                vertex_data.node;
                        }
                    }
                }
                else {
                    VertexData vertex_data = this->add_vertex(
                        node, metadata->get_initial_time_step());
                    if (vertex_data.node->get_metadata()->is_parameter()) {
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

            // Save mapping between the vertice id and it's name.
            this->name_to_id[data.node->get_timed_name()] = vertex_id;

            // Include node as a property of the vertex in the graph.
            this->graph[vertex_id] = data;

            return data;
        }

        void DynamicBayesNet::create_edges() {
            for (const auto& node : this->node_templates) {
                const std::shared_ptr<NodeMetadata> metadata =
                    node.get_metadata();

                for (const auto& parent_link : metadata->get_parent_links()) {
                    if (metadata->is_replicable()) {
                        for (int t = metadata->get_initial_time_step();
                             t < this->time_steps;
                             t++) {

                            this->add_edge(*(parent_link.parent_node),
                                           node,
                                           parent_link.time_crossing,
                                           t);
                        }
                    }
                    else {
                        this->add_edge(*(parent_link.parent_node),
                                       node,
                                       parent_link.time_crossing,
                                       metadata->get_initial_time_step());
                    }
                }
            }
        }

        void DynamicBayesNet::add_edge(const RandomVariableNode& source_node,
                                       const RandomVariableNode& target_node,
                                       bool time_crossing,
                                       int target_time_step) {

            int parent_time_step = -1;
            if (source_node.get_metadata()->is_replicable()) {
                if (time_crossing) {
                    // A replicable node (source) that shows up at time step t-1
                    // and is linked to another node (target) that shpws up at
                    // time step t.
                    if (source_node.get_metadata()->get_initial_time_step() <=
                        target_time_step - 1) {
                        parent_time_step = target_time_step - 1;
                    }
                }
                else {
                    // A replicable node (source) that shows up at time step t
                    // and is linked to another node (target) that also shpws up
                    // at time step t.
                    if (source_node.get_metadata()->get_initial_time_step() <=
                        target_time_step) {
                        parent_time_step = target_time_step;
                    }
                }
            }
            else {
                if (source_node.get_metadata()->is_single_time_link()) {
                    if (time_crossing) {
                        // A non-replicable node (source) that shows up once at
                        // its predefined initial time step (t) and is linked
                        // once to another node (target) that shpws up at time
                        // step t.
                        if (source_node.get_metadata()
                                ->get_initial_time_step() ==
                            target_time_step - 1) {
                            parent_time_step = target_time_step - 1;
                        }
                    }
                    else {
                        // A non-replicable node (source) that shows up once at
                        // its predefined initial time step (t) and is linked
                        // once to another node (target) that shpws up at time
                        // step t+1.
                        if (source_node.get_metadata()
                                ->get_initial_time_step() == target_time_step) {
                            parent_time_step = target_time_step;
                        }
                    }
                }
                else {
                    // A non-replicable node (source) that shows up once at
                    // its predefined initial time step (t) and is linked
                    // to replicas of another node (target) over all time steps
                    // starting at t.
                    if (source_node.get_metadata()->get_initial_time_step() <=
                        target_time_step) {
                        parent_time_step =
                            source_node.get_metadata()->get_initial_time_step();
                    }
                }
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
            for (const auto& node_template : this->node_templates) {
                const std::shared_ptr<NodeMetadata> metadata =
                    node_template.get_metadata();
                if (metadata->has_parameter_parents()) {
                    if (metadata->is_replicable() &&
                        metadata->has_replicable_parameter_parent()) {
                        for (int t = metadata->get_initial_time_step();
                             t < this->time_steps;
                             t++) {
                            VertexData vertex_data =
                                this->graph
                                    [this->name_to_id[node_template
                                                          .get_timed_name(t)]];
                            if (!vertex_data.node->get_cpd()->is_updated()) {
                                vertex_data.node->get_cpd()
                                    ->update_dependencies(parameter_nodes_map,
                                                          t);
                            }
                        }
                    }
                    else {
                        if (!node_template.get_cpd()->is_updated()) {
                            node_template.get_cpd()->update_dependencies(
                                parameter_nodes_map,
                                node_template.get_time_step());
                        }
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

        void DynamicBayesNet::add_node(RandomVariableNode& node) {
            this->node_templates.push_back(node);
        }

        void DynamicBayesNet::add_node(RandomVariableNode&& node) {
            this->node_templates.push_back(std::move(node));
        }

        void DynamicBayesNet::unroll(int time_steps, bool force) {
            if (time_steps != this->time_steps || force) {
                this->time_steps = time_steps;
                NodeMap parameter_nodes_map =
                    this->create_vertices_from_nodes();
                this->create_edges();
                this->update_cpds(parameter_nodes_map);

                for (auto edge = boost::edges(this->graph).first;
                     edge != boost::edges(this->graph).second;
                     edge++) {
                    std::cout << *edge << std::endl;
                }
            }
        }

    } // namespace model
} // namespace tomcat