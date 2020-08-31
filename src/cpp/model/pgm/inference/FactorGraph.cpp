#include "FactorGraph.h"

#include <boost/graph/copy.hpp>
#include <boost/graph/filtered_graph.hpp>
#include <boost/graph/topological_sort.hpp>

#include "../cpd/CPD.h"
#include "FactorNode.h"
#include "NonFactorNode.h"

namespace tomcat {
    namespace model {

        using namespace std;
        using NodeName = ExactInferenceNode::NodeName;

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        FactorGraph::FactorGraph() {}

        FactorGraph::~FactorGraph() {}

        //----------------------------------------------------------------------
        // Copy & Move constructors/assignments
        //----------------------------------------------------------------------
        //        FactorGraph(const FactorGraph& FactorGraph) {}
        //
        //        FactorGraph& operator=(const FactorGraph& FactorGraph) {}

        //----------------------------------------------------------------------
        // Operator overload
        //----------------------------------------------------------------------

        //----------------------------------------------------------------------
        // Static functions
        //----------------------------------------------------------------------


//        CPD::TableOrderingMap FactorGraph::get_unique_labels_ordering_map(
//            const CPD::TableOrderingMap& cpd_ordering_map, int time_step) {
//
//            CPD::TableOrderingMap new_ordering_map;
//
//            for (const auto& [node_label, ordering_map] : cpd_ordering_map) {
//                int parent_time_step = time_step;
//
//                if (node_label == main_node_label) {
//                    // This is a CPD that links two nodes with the same label
//                    // but in different time steps. We should use the parent's
//                    // time step then.
//                }
//                string unique_label =
//                    get_unique_label_for(node_label, time_step);
//                new_ordering_map[unique_label] = ordering_map;
//            }
//
//            return new_ordering_map;
//        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void
        FactorGraph::add_node(const string& node_label,
                              int cardinality,
                              int time_step,
                              Eigen::MatrixXd& cpd,
                              const CPD::TableOrderingMap& cpd_ordering_map) {

            if (time_step > 2) {
                throw TomcatModelException(
                    "A factor graph cannot contain "
                    "nodes at time step greater than 2.");
            }

            this->last_time_step = max(time_step, this->last_time_step);

            // Include a non factor node in the graph and his incoming
            // factor node, that will contain the node's CPD in form of
            // potential function.

            int target_id =
                this->add_non_factor_node(node_label, cardinality, time_step);
            int source_id = this->add_factor_node(
                node_label, time_step, cpd, cpd_ordering_map);

            boost::add_edge(source_id, target_id, this->graph);
        }

        int FactorGraph::add_non_factor_node(const string& node_label,
                                             int cardinality,
                                             int time_step) {

            string unique_label = ExactInferenceNode::get_unique_label_for(node_label, time_step);

            int vertex_id = boost::add_vertex(this->graph);
            VertexData vertex;
            vertex.node = make_shared<NonFactorNode>(unique_label, cardinality);
            vertex.factor = false;
            vertex.time_step = time_step;
            this->graph[vertex_id] = vertex;

            string node_name = NodeName::get(unique_label, time_step);
            this->name_to_id[node_name] = vertex_id;

            return vertex_id;
        }

        int FactorGraph::add_factor_node(
            const std::string& node_label,
            int time_step,
            Eigen::MatrixXd& cpd,
            const CPD::TableOrderingMap& cpd_ordering_map) {

            string unique_label = ExactInferenceNode::get_unique_label_for(node_label, time_step);
            string factor_label = this->get_factor_label(unique_label);

            int vertex_id = boost::add_vertex(this->graph);
            VertexData vertex;
//            CPD::TableOrderingMap new_ordering_map =
//                get_unique_labels_ordering_map(cpd_ordering_map, time_step);
            vertex.node =
                make_shared<FactorNode>(factor_label, cpd, cpd_ordering_map);
            vertex.factor = true;
            vertex.time_step = time_step;
            this->graph[vertex_id] = vertex;

            string factor_name = NodeName::get(factor_label, time_step);
            this->name_to_id[factor_name] = vertex_id;

            return vertex_id;
        }

        string FactorGraph::get_factor_label(
            const string& target_non_factor_label) const {
            return "f:" + target_non_factor_label;
        }

        void FactorGraph::add_edge(const string& source_node_label,
                                   int source_node_time_step,
                                   const string& target_node_label,
                                   int target_node_time_step) {

            if (source_node_time_step > this->last_time_step ||
                target_node_time_step > this->last_time_step) {
                throw TomcatModelException(
                    "There are no vertices in the time step informed.");
            }

            // When adding an edge, the target node must be actually the
            // incoming factor node of it. Since the CPD of a node is given by a
            // joint distribution of its parents, its guaranteed to have only
            // one incoming factor node per non-factor node in the graph.
            string unique_source_label =
                ExactInferenceNode::get_unique_label_for(source_node_label, source_node_time_step);
            string unique_target_label =
                ExactInferenceNode::get_unique_label_for(this->get_factor_label(target_node_label),
                                     target_node_time_step);

            string source_node_name =
                NodeName::get(unique_source_label, source_node_time_step);
            string target_node_name =
                NodeName::get(unique_target_label, target_node_time_step);

            int source_vertex_id = this->name_to_id[source_node_name];
            int target_vertex_id = this->name_to_id[target_node_name];

            boost::add_edge(source_vertex_id, target_vertex_id, this->graph);

            dynamic_pointer_cast<FactorNode>(this->graph[target_vertex_id].node)->replace_cpd_ordering_label(
                source_node_label, unique_source_label);
        }

        void FactorGraph::compute_topological_traversal_per_time_slice() {
            using V = FactorGraph::Graph::vertex_descriptor;
            using Filtered = boost::
                filtered_graph<Graph, boost::keep_all, function<bool(V)>>;

            Graph::edge_iterator begin, end;
            boost::tie(begin, end) = boost::edges(this->graph);
            while (begin != end) {
                int source = boost::source(*begin, this->graph);
                int target = boost::target(*begin, this->graph);
                LOG(this->graph[source].node->get_label() + " -> " +
                    this->graph[target].node->get_label());
                begin++;
            }

            for (int t = 0; t <= this->last_time_step; t++) {
                Filtered time_sliced_graph(
                    this->graph, boost::keep_all{}, [&](V v) {
                        return this->graph[v].time_step == t;
                    });

                Filtered::edge_iterator begin, end;
                boost::tie(begin, end) = boost::edges(time_sliced_graph);
                while (begin != end) {
                    int source = boost::source(*begin, this->graph);
                    int target = boost::target(*begin, this->graph);
                    LOG(this->graph[source].node->get_label() + " -> " +
                        this->graph[target].node->get_label());
                    begin++;
                }

                vector<int> vertex_ids_in_topol_order;
                boost::topological_sort(
                    time_sliced_graph,
                    back_inserter(vertex_ids_in_topol_order));

                int num_vertices_in_time_slice =
                    vertex_ids_in_topol_order.size();
                this->time_sliced_topological_order[t] =
                    vector<VertexData>(num_vertices_in_time_slice);
                this->time_sliced_reversed_topological_order[t] =
                    vector<VertexData>(num_vertices_in_time_slice);

                for (int i = 0; i < num_vertices_in_time_slice; i++) {
                    const VertexData& data =
                        this->graph[vertex_ids_in_topol_order[i]];

                    this->time_sliced_topological_order
                        [t][num_vertices_in_time_slice - i - 1] = data;
                    this->time_sliced_reversed_topological_order[t][i] = data;
                }
            }
        }

        vector<FactorGraph::VertexData>
        FactorGraph::get_vertices_topological_order(
            int time_step, bool from_roots_to_leaves) const {

            int relative_time_step = min(this->last_time_step, time_step);

            if (from_roots_to_leaves) {
                return this->time_sliced_topological_order[relative_time_step];
            }
            else {
                return this->time_sliced_reversed_topological_order
                    [relative_time_step];
            }
        }

        vector<FactorGraph::VertexData>
        FactorGraph::get_parents_of(const NodeName& node_name) const {

            string relative_name = this->get_relative_name_for(node_name);
            int vertex_id = this->name_to_id.at(relative_name);
            Graph::in_edge_iterator in_begin, in_end;
            boost::tie(in_begin, in_end) = in_edges(vertex_id, this->graph);

            vector<VertexData> parent_vertices;
            while (in_begin != in_end) {
                int parent_vertex_id = source(*in_begin, graph);
                parent_vertices.push_back(this->graph[parent_vertex_id]);
                in_begin++;
            }

            return parent_vertices;
        }

        string
        FactorGraph::get_relative_name_for(const NodeName& node_name) const {
            // All nodes beyont the last time step represented by the factor
            // graph are represented by the node with the same label in that
            // last time step.
            int relative_time_step =
                min(node_name.time_step, this->last_time_step);
            return NodeName::get(node_name.label, relative_time_step);
        }

        vector<FactorGraph::VertexData>
        FactorGraph::get_children_of(const NodeName& node_name) const {

            string relative_name = this->get_relative_name_for(node_name);
            int vertex_id = this->name_to_id.at(relative_name);
            Graph::out_edge_iterator out_begin, out_end;
            boost::tie(out_begin, out_end) = out_edges(vertex_id, this->graph);

            vector<VertexData> child_vertices;
            while (out_begin != out_end) {
                int child_vertex_id = target(*out_begin, graph);
                child_vertices.push_back(this->graph[child_vertex_id]);
                out_begin++;
            }

            return child_vertices;
        }

        Eigen::MatrixXd
        FactorGraph::get_marginal_for(const NodeName& node_name) const {
            Eigen::MatrixXd marginal(0, 0);

            string relative_name = this->get_relative_name_for(node_name);
            if (EXISTS(relative_name, this->name_to_id)) {
                int vertex_id = this->name_to_id.at(relative_name);

                if (this->graph[vertex_id].factor) {
                    throw TomcatModelException(
                        "Factor nodes do not have a marginal distribution.");
                }

                marginal = dynamic_pointer_cast<NonFactorNode>(
                               this->graph[vertex_id].node)
                               ->get_marginal_at(node_name.time_step);
            }

            return marginal;
        }

        //----------------------------------------------------------------------
        // Getters & Setters
        //----------------------------------------------------------------------

    } // namespace model
} // namespace tomcat
