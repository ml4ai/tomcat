#include "DynamicBayesNet.h"

#include <boost/filesystem.hpp>
#include <boost/graph/topological_sort.hpp>

#include "../utils/FileHandler.h"
#include "ConstantNode.h"

namespace tomcat {
    namespace model {
        //----------------------------------------------------------------------
        // Definitions
        //----------------------------------------------------------------------

        // No definitions in this file

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        DynamicBayesNet::DynamicBayesNet() {}

        DynamicBayesNet::DynamicBayesNet(int num_node_templates) {
            this->node_templates.reserve(num_node_templates);
        }

        DynamicBayesNet::~DynamicBayesNet() {}

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void
        DynamicBayesNet::add_node_template(const RandomVariableNode& node) {
            RandomVariableNode cloned_node =
                *(dynamic_cast<RandomVariableNode*>(node.clone().get()));
            this->node_templates.push_back(cloned_node);
        }

        void DynamicBayesNet::unroll(int time_steps, bool force) {
            if (time_steps != this->time_steps || force) {
                this->reset();
                this->time_steps = time_steps;
                this->create_vertices_from_nodes();
                this->create_edges();
                this->update_cpd_templates_dependencies();
                this->set_nodes_cpd();

                for (auto edge = boost::edges(this->graph).first;
                     edge != boost::edges(this->graph).second;
                     edge++) {
                    std::cout << *edge << std::endl;
                }
            }
        }

        void DynamicBayesNet::reset() {
            this->time_steps = 0;
            this->graph.clear();
            this->name_to_id.clear();
            this->parameter_nodes_map.clear();
            this->label_to_nodes.clear();
            this->nodes.clear();
        }

        void DynamicBayesNet::create_vertices_from_nodes() {
            for (const auto& node_template : this->node_templates) {
                const std::shared_ptr<NodeMetadata> metadata =
                    node_template.get_metadata();
                if (metadata->is_replicable()) {
                    for (int t = metadata->get_initial_time_step();
                         t < this->time_steps;
                         t++) {

                        VertexData vertex_data =
                            this->add_vertex(node_template, t);

                        if (vertex_data.node->get_metadata()->is_parameter()) {
                            std::string node_name =
                                vertex_data.node->get_timed_name();
                            this->parameter_nodes_map[node_name] =
                                vertex_data.node;
                        }
                    }
                }
                else {
                    VertexData vertex_data = this->add_vertex(
                        node_template, metadata->get_initial_time_step());
                    if (vertex_data.node->get_metadata()->is_parameter()) {
                        this->parameter_nodes_map[vertex_data.node
                                                      ->get_timed_name()] =
                            vertex_data.node;
                    }
                }
            }
        }

        VertexData
        DynamicBayesNet::add_vertex(const RandomVariableNode& node_template,
                                    int time_step) {
            int vertex_id = boost::add_vertex(this->graph);

            VertexData data;
            data.node = std::make_shared<RandomVariableNode>(node_template);
            data.node->set_time_step(time_step);

            if (data.node->get_metadata()->has_replicable_parameter_parent()) {
                // If a node has parameter nodes that are replicable, its
                // replicas do not share the same CPD's and therefore it cannot
                // use the CPD pointer inherited from its template.
                data.node->clone_cpd_templates();
            }

            // Save mapping between the vertice id and it's name.
            this->name_to_id[data.node->get_timed_name()] = vertex_id;

            // Include node as a property of the vertex in the graph.
            this->graph[vertex_id] = data;

            // Include node in the list of created nodes
            this->label_to_nodes[data.node->get_metadata()->get_label()]
                .push_back(data.node);

            this->nodes.push_back(data.node);

            return data;
        }

        void DynamicBayesNet::create_edges() {
            for (const auto& node_template : this->node_templates) {
                const std::shared_ptr<NodeMetadata> metadata =
                    node_template.get_metadata();

                for (const auto& parent_link : metadata->get_parent_links()) {
                    if (metadata->is_replicable()) {
                        for (int t = metadata->get_initial_time_step();
                             t < this->time_steps;
                             t++) {

                            this->add_edge(*(parent_link.parent_node_metadata),
                                           *(node_template.get_metadata()),
                                           parent_link.time_crossing,
                                           t);
                        }
                    }
                    else {
                        this->add_edge(*(parent_link.parent_node_metadata),
                                       *(node_template.get_metadata()),
                                       parent_link.time_crossing,
                                       metadata->get_initial_time_step());
                    }
                }
            }
        }

        void DynamicBayesNet::add_edge(const NodeMetadata& source_node_metadata,
                                       const NodeMetadata& target_node_metadata,
                                       bool time_crossing,
                                       int target_time_step) {

            int parent_time_step = -1;
            if (source_node_metadata.is_replicable()) {
                if (time_crossing) {
                    // A replicable node (source) that shows up at time step t-1
                    // and is linked to another node (target) that shpws up at
                    // time step t.
                    if (source_node_metadata.get_initial_time_step() <=
                        target_time_step - 1) {
                        parent_time_step = target_time_step - 1;
                    }
                }
                else {
                    // A replicable node (source) that shows up at time step t
                    // and is linked to another node (target) that also shpws up
                    // at time step t.
                    if (source_node_metadata.get_initial_time_step() <=
                        target_time_step) {
                        parent_time_step = target_time_step;
                    }
                }
            }
            else {
                if (source_node_metadata.is_single_time_link()) {
                    if (time_crossing) {
                        // A non-replicable node (source) that shows up once at
                        // its predefined initial time step (t) and is linked
                        // once to another node (target) that shpws up at time
                        // step t.
                        if (source_node_metadata.get_initial_time_step() ==
                            target_time_step - 1) {
                            parent_time_step = target_time_step - 1;
                        }
                    }
                    else {
                        // A non-replicable node (source) that shows up once at
                        // its predefined initial time step (t) and is linked
                        // once to another node (target) that shpws up at time
                        // step t+1.
                        if (source_node_metadata.get_initial_time_step() ==
                            target_time_step) {
                            parent_time_step = target_time_step;
                        }
                    }
                }
                else {
                    // A non-replicable node (source) that shows up once at
                    // its predefined initial time step (t) and is linked
                    // to replicas of another node (target) over all time steps
                    // starting at t.
                    if (source_node_metadata.get_initial_time_step() <=
                        target_time_step) {
                        parent_time_step =
                            source_node_metadata.get_initial_time_step();
                    }
                }
            }

            if (parent_time_step >= 0) {
                int source_vertex_id = this->name_to_id.at(
                    source_node_metadata.get_timed_name(parent_time_step));
                int target_vertex_id = this->name_to_id.at(
                    target_node_metadata.get_timed_name(target_time_step));
                boost::add_edge(
                    source_vertex_id, target_vertex_id, this->graph);
            }
        }

        void DynamicBayesNet::set_nodes_cpd() {
            for (auto& node : this->nodes) {
                std::vector<std::shared_ptr<Node>> parent_nodes =
                    this->get_parent_nodes_of(node, true);
                std::vector<std::string> parent_labels;
                parent_labels.reserve(parent_nodes.size());

                for (const auto& parent_node : parent_nodes) {
                    std::string label =
                        parent_node->get_metadata()->get_label();
                    parent_labels.push_back(label);
                }

                std::shared_ptr<RandomVariableNode> rv_node =
                    std::dynamic_pointer_cast<RandomVariableNode>(node);
                std::shared_ptr<CPD> cpd = rv_node->get_cpd_for(parent_labels);
                rv_node->set_cpd(cpd);
                rv_node->reset_cpd_updated_status();
            }
        }

        void DynamicBayesNet::update_cpd_templates_dependencies() {
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
                                node_template.get_metadata()->get_timed_name(
                                    t));
                            VertexData vertex_data = this->graph[vertex_id];
                            vertex_data.node->update_cpd_templates_dependencies(
                                this->parameter_nodes_map, t);
                        }
                    }
                    else {
                        int t = node_template.get_metadata()
                                    ->get_initial_time_step();
                        int vertex_id = this->name_to_id.at(
                            node_template.get_metadata()->get_timed_name(t));
                        VertexData vertex_data = this->graph[vertex_id];
                        vertex_data.node->update_cpd_templates_dependencies(
                            this->parameter_nodes_map, t);
                    }
                }
            }
        }

        void DynamicBayesNet::check() {
            // TODO - Implement the verifications needed to make sure the DBN is
            //  valid and prepared to be unrolled.
            //  Only allow conjugate priors
        }

        std::vector<std::shared_ptr<Node>>
        DynamicBayesNet::get_parameter_nodes() {
            std::vector<std::shared_ptr<Node>> parameter_nodes;
            parameter_nodes.reserve(this->parameter_nodes_map.size());

            for (const auto& mapping : this->parameter_nodes_map) {
                parameter_nodes.push_back(mapping.second);
            }

            return parameter_nodes;
        }

        std::vector<std::shared_ptr<Node>> DynamicBayesNet::get_nodes_by_label(
            const std::string& node_label) const {
            return this->label_to_nodes.at(node_label);
        }

        std::vector<std::shared_ptr<Node>>
        DynamicBayesNet::get_nodes_topological_order(
            bool from_roots_to_leaves) const {
            std::vector<int> vertex_ids;
            boost::topological_sort(this->graph,
                                    std::back_inserter(vertex_ids));

            std::vector<std::shared_ptr<Node>> nodes(vertex_ids.size());
            int i = 0;
            if (from_roots_to_leaves) {
                // The default behavior of the boost topological sort is to
                // order from the leaves to the roots so if we want otherwise,
                // the elements have to be inserted in reversed order in the
                // final array.
                i = vertex_ids.size() - 1;
            }

            for (int vertex_id : vertex_ids) {
                if (from_roots_to_leaves) {
                    nodes[i--] = this->graph[vertex_id].node;
                }
                else {
                    nodes[i++] = this->graph[vertex_id].node;
                }
            }

            return nodes;
        }

        std::vector<std::shared_ptr<Node>>
        DynamicBayesNet::get_parent_nodes_of(const std::shared_ptr<Node>& node,
                                             bool exclude_parameters) const {

            int vertex_id = this->name_to_id.at(node->get_timed_name());
            std::vector<std::shared_ptr<Node>> parent_nodes;

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

        std::vector<std::shared_ptr<Node>> DynamicBayesNet::get_child_nodes_of(
            const std::shared_ptr<Node>& node) const {

            int vertex_id = this->name_to_id.at(node->get_timed_name());
            std::vector<std::shared_ptr<Node>> child_nodes;

            Graph::out_edge_iterator out_begin, out_end;
            boost::tie(out_begin, out_end) = out_edges(vertex_id, this->graph);
            while (out_begin != out_end) {
                int child_vertex_id = target(*out_begin, graph);
                child_nodes.push_back(this->graph[child_vertex_id].node);
                out_begin++;
            }

            return child_nodes;
        }

        void DynamicBayesNet::save_to_folder(
            const std::string& output_folder) const {

            for (const auto& mapping : this->parameter_nodes_map) {
                std::string filename = mapping.first + ".txt";
                std::string filepath = get_filepath(output_folder, filename);
                save_matrix_to_file(filepath, mapping.second->get_assignment());
            }
        }

        void
        DynamicBayesNet::load_from_folder(const std::string& input_folder) {
            for (const auto& file :
                 boost::filesystem::directory_iterator(input_folder)) {

                std::string filename = file.path().filename().string();
                std::string parameter_timed_name = remove_extension(filename);
                Eigen::VectorXd assignment =
                    read_matrix_from_file(file.path().string());

                // Set loaded vector as assignment of the corresponding
                // parameter node.
                RandomVariableNode* parameter_node =
                    dynamic_cast<RandomVariableNode*>(
                        this->parameter_nodes_map.at(parameter_timed_name)
                            .get());
                parameter_node->set_assignment(assignment);
                parameter_node->freeze();
            }
        }

        //------------------------------------------------------------------
        // Getters & Setters
        //------------------------------------------------------------------
        //        const std::vector<std::shared_ptr<RandomVariableNode>>
        //        DynamicBayesNet::get_nodes() const {
        //            return nodes;
        //        }
        //
        //        const std::vector<RandomVariableNode>&
        //        DynamicBayesNet::get_node_templates() const {
        //            return node_templates;
        //        }

        int DynamicBayesNet::get_time_steps() const { return time_steps; }

    } // namespace model
} // namespace tomcat