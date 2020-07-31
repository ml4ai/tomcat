#include "DynamicBayesNet.h"
#include "CPD.h"
#include <boost/graph/topological_sort.hpp>
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
            data.node = std::make_shared<RandomVariableNode>(node);
            data.node->set_time_step(time_step);
            data.node->reset_cpds_updated_status();

            if (data.node->get_metadata()->has_replicable_parameter_parent()) {
                // If a node has parameter nodes that are replicable, its
                // replicas do not share the same CPD's and therefore it cannot
                // use the CPD pointer inherited from its template.
                data.node->clone_cpds();
            }

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
                int source_vertex_id = this->name_to_id.at(
                    source_node.get_timed_name(parent_time_step));
                int target_vertex_id = this->name_to_id.at(
                    target_node.get_timed_name(target_time_step));
                boost::add_edge(
                    source_vertex_id, target_vertex_id, this->graph);
            }
        }

        std::vector<std::string>
        DynamicBayesNet::get_parent_labels_of(const RandomVariableNode& node) {
            std::vector<std::string> parent_labels;
            for (const auto& parent_node :
                 this->get_parent_nodes_of(node, true)) {
                parent_labels.push_back(
                    parent_node->get_metadata()->get_label());
            }

            return parent_labels;
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

                            int vertex_id = this->name_to_id.at(
                                node_template.get_timed_name(t));
                            VertexData vertex_data = this->graph[vertex_id];
                            vertex_data.node->update_cpd_dependencies(
                                parameter_nodes_map, t);

                            //                            std::shared_ptr<CPD>
                            //                            cpd =
                            //                                vertex_data.node->get_cpd_for(
                            //                                    this->get_parent_labels_of(
                            //                                        *(vertex_data.node)));
                            //
                            //                            if
                            //                            (!cpd->is_updated()) {
                            //                                cpd->update_dependencies(parameter_nodes_map,
                            //                                                         t);
                            //                            }
                        }
                    }
                    else {
                        int t = node_template.get_metadata()
                                    ->get_initial_time_step();
                        int vertex_id = this->name_to_id.at(
                            node_template.get_timed_name(t));
                        VertexData vertex_data = this->graph[vertex_id];
                        vertex_data.node->update_cpd_dependencies(
                            parameter_nodes_map, t);

                        //                        std::shared_ptr<CPD> cpd =
                        //                            vertex_data.node->get_cpd_for(
                        //                                this->get_parent_labels_of(
                        //                                    *(vertex_data.node)));
                        //
                        //                        if (!cpd->is_updated()) {
                        //                            cpd->update_dependencies(parameter_nodes_map,
                        //                            t);
                        //                        }
                    }
                }
            }
        }

        DynamicBayesNet::DynamicBayesNet(int num_nodes) {
            this->node_templates.reserve(num_nodes);
        }

        void DynamicBayesNet::add_node_template(RandomVariableNode& node) {
            this->node_templates.push_back(node);
        }

        void DynamicBayesNet::add_node_template(RandomVariableNode&& node) {
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

        void DynamicBayesNet::check() {
            // todo
        }

        std::vector<std::shared_ptr<RandomVariableNode>>
        DynamicBayesNet::get_nodes_topological_order() const {
            std::vector<int> vertex_ids;
            boost::topological_sort(this->graph,
                                    std::back_inserter(vertex_ids));

            std::vector<std::shared_ptr<RandomVariableNode>> nodes(
                vertex_ids.size());
            int i = vertex_ids.size();

            for (int vertex_id : vertex_ids) {
                nodes[--i] = this->graph[vertex_id].node;
            }

            return nodes;
        }

        std::vector<std::shared_ptr<RandomVariableNode>>
        DynamicBayesNet::get_parent_nodes_of(const RandomVariableNode& node,
                                             bool exclude_parameters) const {

            int vertex_id = this->name_to_id.at(node.get_timed_name());
            std::vector<std::shared_ptr<RandomVariableNode>> parent_nodes;

            Graph::in_edge_iterator in_begin, in_end;
            boost::tie(in_begin, in_end) = in_edges(vertex_id, this->graph);
            while (in_begin != in_end) {
                int parent_vertex_id = source(*in_begin, graph);
                if (!this->graph[parent_vertex_id]
                         .node->get_metadata()
                         ->is_parameter() ||
                    !exclude_parameters) {
                    parent_nodes.push_back(this->graph[parent_vertex_id].node);
                }
                in_begin++;
            }

            return parent_nodes;
        }

        void DynamicBayesNet::save_to_folder(const std::string& output_directory) const {
            // todo
        }

        void DynamicBayesNet::load_from_folder(const std::string& input_directory) {
            // todo
        }

    } // namespace model
} // namespace tomcat