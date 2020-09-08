#include "FactorGraph.h"

#include <boost/graph/filtered_graph.hpp>
#include <boost/graph/topological_sort.hpp>

#include "../cpd/CPD.h"
#include "FactorNode.h"
#include "VariableNode.h"

namespace tomcat {
    namespace model {

        using namespace std;

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        FactorGraph::FactorGraph() {}

        FactorGraph::~FactorGraph() {}

        //----------------------------------------------------------------------
        // Static functions
        //----------------------------------------------------------------------
        FactorGraph
        FactorGraph::create_from_unrolled_dbn(const DynamicBayesNet& dbn) {
            FactorGraph factor_graph;

            for (const auto& node : dbn.get_nodes_topological_order()) {
                if (!node->get_metadata()->is_parameter()) {
                    shared_ptr<RandomVariableNode> random_variable =
                        dynamic_pointer_cast<RandomVariableNode>(node);

                    if (random_variable->get_time_step() <= 2) {
                        factor_graph.add_node(
                            random_variable->get_metadata()->get_label(),
                            random_variable->get_metadata()->get_cardinality(),
                            random_variable->get_time_step(),
                            random_variable->get_cpd()->get_table(),
                            random_variable->get_cpd()
                                ->get_parent_label_to_indexing());
                    }
                }
            }

            for (const auto& [source_node, target_node] : dbn.get_edges()) {
                if (source_node->get_time_step() <= 2 &&
                    !source_node->get_metadata()->is_parameter() &&
                    target_node->get_time_step() <= 2 &&
                    !target_node->get_metadata()->is_parameter()) {

                    factor_graph.add_edge(
                        source_node->get_metadata()->get_label(),
                        source_node->get_time_step(),
                        target_node->get_metadata()->get_label(),
                        target_node->get_time_step());
                }
            }

            return factor_graph;
        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void
        FactorGraph::add_node(const string& node_label,
                              int cardinality,
                              int time_step,
                              const Eigen::MatrixXd& cpd,
                              const CPD::TableOrderingMap& cpd_ordering_map) {

            if (time_step > 2) {
                throw TomcatModelException("A factor graph cannot have nodes "
                                           "at time step greater than 2.");
            }

            this->repeatable_time_step =
                max(time_step, this->repeatable_time_step);

            // Include a non factor node in the graph and his parent factor
            // node, that will contain the node's CPD in form of potential
            // function.
            int target_id =
                this->add_variable_node(node_label, cardinality, time_step);
            int source_id = this->add_factor_node(
                node_label, time_step, cpd, cpd_ordering_map);

            boost::add_edge(source_id, target_id, this->graph);
        }

        int FactorGraph::add_variable_node(const string& node_label,
                                           int cardinality,
                                           int time_step) {

            int vertex_id = boost::add_vertex(this->graph);
            this->graph[vertex_id] =
                make_shared<VariableNode>(node_label, time_step, cardinality);

            string node_name = this->graph[vertex_id]->get_name();
            this->name_to_id[node_name] = vertex_id;

            return vertex_id;
        }

        int FactorGraph::add_factor_node(
            const string& node_label,
            int time_step,
            const Eigen::MatrixXd& cpd,
            const CPD::TableOrderingMap& cpd_ordering_map) {

            int vertex_id = boost::add_vertex(this->graph);
            this->graph[vertex_id] = make_shared<FactorNode>(
                node_label, time_step, cpd, cpd_ordering_map, node_label);

            string factor_name = this->graph[vertex_id]->get_name();
            this->name_to_id[factor_name] = vertex_id;

            return vertex_id;
        }

        void FactorGraph::add_edge(const string& source_node_label,
                                   int source_node_time_step,
                                   const string& target_node_label,
                                   int target_node_time_step) {

            if (source_node_time_step > this->repeatable_time_step ||
                target_node_time_step > this->repeatable_time_step) {
                stringstream ss;
                ss << "It's not possible to define connections between nodes "
                      "with time step greater than "
                   << this->repeatable_time_step;
                throw TomcatModelException(ss.str());
            }

            // When adding an edge between two variable nodes, the target node
            // must be the parent factor of the target variable node. Since the
            // CPD of a variable node is given by a joint distribution of its
            // parents, it's guaranteed to have only one parent factor node per
            // variable node in the graph.
            string source_node_name =
                MessageNode::get_name(source_node_label, source_node_time_step);
            string target_factor_label =
                FactorNode::compose_label(target_node_label);
            string target_node_name = MessageNode::get_name(
                target_factor_label, target_node_time_step);

            int source_vertex_id = this->name_to_id[source_node_name];
            int target_vertex_id = this->name_to_id[target_node_name];

            boost::add_edge(source_vertex_id, target_vertex_id, this->graph);

            if (target_node_time_step > source_node_time_step) {
                shared_ptr<FactorNode> factor_node =
                    dynamic_pointer_cast<FactorNode>(
                        this->graph[target_vertex_id]);
            }
        }

        void FactorGraph::store_topological_traversal_per_time_step() {
            using V = FactorGraph::Graph::vertex_descriptor;
            using Filtered = boost::
                filtered_graph<Graph, boost::keep_all, function<bool(V)>>;

            for (int t = 0; t <= this->repeatable_time_step; t++) {
                Filtered time_sliced_graph(
                    this->graph, boost::keep_all{}, [&](V v) {
                        return this->graph[v]->get_time_step() == t;
                    });

                vector<int> vertex_ids_in_topol_order;
                boost::topological_sort(
                    time_sliced_graph,
                    back_inserter(vertex_ids_in_topol_order));

                int num_vertices_in_time_slice =
                    vertex_ids_in_topol_order.size();
                this->time_sliced_topological_order[t] =
                    vector<shared_ptr<MessageNode>>(num_vertices_in_time_slice);
                this->time_sliced_reversed_topological_order[t] =
                    vector<shared_ptr<MessageNode>>(num_vertices_in_time_slice);

                for (int i = 0; i < num_vertices_in_time_slice; i++) {
                    shared_ptr<MessageNode> node =
                        this->graph[vertex_ids_in_topol_order[i]];

                    this->time_sliced_topological_order
                        [t][num_vertices_in_time_slice - i - 1] = node;
                    this->time_sliced_reversed_topological_order[t][i] = node;
                }
            }
        }

        vector<shared_ptr<MessageNode>>
        FactorGraph::get_vertices_topological_order_in(
            int time_step, bool from_roots_to_leaves) const {

            int relative_time_step = min(this->repeatable_time_step, time_step);

            if (from_roots_to_leaves) {
                return this->time_sliced_topological_order[relative_time_step];
            }
            else {
                return this->time_sliced_reversed_topological_order
                    [relative_time_step];
            }
        }

        vector<pair<shared_ptr<MessageNode>, bool>> FactorGraph::get_parents_of(
            const shared_ptr<MessageNode>& template_node, int time_step) const {

            int vertex_id = this->name_to_id.at(template_node->get_name());
            Graph::in_edge_iterator in_begin, in_end;
            boost::tie(in_begin, in_end) = in_edges(vertex_id, this->graph);

            vector<pair<shared_ptr<MessageNode>, bool>> parent_nodes;
            while (in_begin != in_end) {
                int parent_vertex_id = source(*in_begin, graph);
                shared_ptr<MessageNode> parent_node =
                    this->graph[parent_vertex_id];
                bool transition = false;
                if (parent_node->get_time_step() <
                    template_node->get_time_step()) {

                    transition = true;

                    if (time_step > this->repeatable_time_step) {
                        string real_parent_name =
                            MessageNode::get_name(parent_node->get_label(),
                                                  this->repeatable_time_step);
                        int real_parent_vertex_id =
                            this->name_to_id.at(real_parent_name);
                        parent_node = this->graph[real_parent_vertex_id];
                    }
                }
                parent_nodes.push_back(make_pair(parent_node, transition));
                in_begin++;
            }

            return parent_nodes;
        }

        vector<shared_ptr<MessageNode>> FactorGraph::get_children_of(
            const shared_ptr<MessageNode>& template_node) const {

            int vertex_id = this->name_to_id.at(template_node->get_name());
            Graph::out_edge_iterator out_begin, out_end;
            boost::tie(out_begin, out_end) = out_edges(vertex_id, this->graph);

            vector<shared_ptr<MessageNode>> child_nodes;
            while (out_begin != out_end) {
                int child_vertex_id = target(*out_begin, graph);
                child_nodes.push_back(this->graph[child_vertex_id]);
                out_begin++;
            }

            return child_nodes;
        }

        Eigen::MatrixXd
        FactorGraph::get_marginal_for(const string& node_label,
                                      int time_step) const {
            Eigen::MatrixXd marginal(0, 0);

            string relative_name = MessageNode::get_name(
                node_label, min(time_step, this->repeatable_time_step));
            if (EXISTS(relative_name, this->name_to_id)) {
                int vertex_id = this->name_to_id.at(relative_name);

                if (this->graph[vertex_id]->is_factor()) {
                    throw TomcatModelException(
                        "Factor nodes do not have a marginal distribution.");
                }

                marginal =
                    dynamic_pointer_cast<VariableNode>(this->graph[vertex_id])
                        ->get_marginal_at(time_step);
            }

            return marginal;
        }

        void FactorGraph::erase_incoming_messages_beyond(int time_step) {
            for (int t = min(this->repeatable_time_step, time_step + 1);
                 t <= this->repeatable_time_step;
                 t++) {
                for (auto& node : time_sliced_topological_order.at(t)) {
                    node->erase_incoming_messages_beyond(time_step);
                }
            }
        }

    } // namespace model
} // namespace tomcat
