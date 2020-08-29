#include "FactorGraph.h"

#include <boost/graph/subgraph.hpp>
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

            // Include a non factor node in the graph and his incoming factor
            // node, that will contain the node's CPD in form of potential
            // function.
            int non_factor_vertex_id = boost::add_vertex(*(this->graph));
            VertexData non_factor_vertex;
            non_factor_vertex.node =
                make_shared<NonFactorNode>(node_label, cardinality);
            non_factor_vertex.factor = false;
            non_factor_vertex.time_step = time_step;
            this->graph[non_factor_vertex_id] = non_factor_vertex;

            int incoming_factor_vertex_id = boost::add_vertex(*(this->graph));
            string factor_label = this->get_factor_label(node_label);
            VertexData incoming_factor_vertex;
            incoming_factor_vertex.node =
                make_shared<FactorNode>(factor_label, cpd, cpd_ordering_map);
            incoming_factor_vertex.factor = true;
            incoming_factor_vertex.time_step = time_step;
            this->graph[incoming_factor_vertex_id] = incoming_factor_vertex;

            // Store mapping between node's name and id for fast access to a
            // vertex's property.
            string non_factor_name = NodeName::get(node_label, time_step);
            this->name_to_id[non_factor_name] = non_factor_vertex_id;
            string factor_name = NodeName::get(factor_label, time_step);
            this->name_to_id[factor_name] = incoming_factor_vertex_id;

            // Store the vertex ids in the list of vertices for the sub-graph
            // that is a slice of the complete graph at the time step informed.
            this->time_sliced_vertices[time_step].push_back(
                non_factor_vertex_id);
            this->time_sliced_vertices[time_step].push_back(
                incoming_factor_vertex_id);
        }

        string FactorGraph::get_factor_label(
            const string& target_non_factor_label) const {
            return "f:" + target_non_factor_label;
        }

        void FactorGraph::add_edge(const string& source_node_label,
                                   int source_node_time_step,
                                   const string& target_node_label,
                                   int target_node_time_step) {

            // When adding an edge, the target node must be actually the
            // incoming factor node of it. Since the CPD of a node is given by a
            // joint distribution of its parents, its guaranteed to have only
            // one incoming factor node per non-factor node in the graph.
            string target_factor_label =
                this->get_factor_label(target_node_label);

            string source_node_name =
                NodeName::get(source_node_label, source_node_time_step);
            string target_node_name =
                NodeName::get(target_factor_label, target_node_time_step);

            int source_vertex_id = this->name_to_id[source_node_name];
            int target_vertex_id = this->name_to_id[target_node_name];

            boost::add_edge(source_vertex_id, target_vertex_id, this->graph);
        }

        void FactorGraph::compute_topological_traversal_per_time_slice() {
            for (int t = 0; t < this->last_time_step; t++) {
                Graph& subgraph = this->graph.create_subgraph();
                for (const auto& vertex_id : this->time_sliced_vertices[t]) {
                    boost::add_vertex(vertex_id, subgraph);
                }

                vector<int> vertex_ids_in_topol_order;
                boost::topological_sort(
                    subgraph, front_inserter(vertex_ids_in_topol_order));

                int num_vertices_in_time_slice =
                    vertex_ids_in_topol_order.size();
                this->time_sliced_topological_order[t] =
                    vector<VertexData>(num_vertices_in_time_slice);
                this->time_sliced_reversed_topological_order[t] =
                    vector<VertexData>(num_vertices_in_time_slice);
                for (int i = 0; i < num_vertices_in_time_slice; i++) {
                    const VertexData& data =
                        this->graph[vertex_ids_in_topol_order[i]];

                    this->time_sliced_topological_order[t][i] = data;
                    this->time_sliced_reversed_topological_order
                        [t][num_vertices_in_time_slice - i - 1] = data;
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

        string FactorGraph::get_relative_name_for(const NodeName& node_name) const {
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

        Eigen::VectorXd FactorGraph::get_marginal_for(const NodeName& node_name) const {
            string relative_name = this->get_relative_name_for(node_name);
            int vertex_id = this->name_to_id.at(relative_name);

            if (this->graph[vertex_id].factor) {
                throw TomcatModelException(
                    "Factor nodes do not have a marginal distribution.");
            }

            Eigen::VectorXd marginal =
                dynamic_pointer_cast<NonFactorNode>(this->graph[vertex_id])
                    ->get_marginal_at(node_name.time_step);

            return marginal;
        }

        //----------------------------------------------------------------------
        // Getters & Setters
        //----------------------------------------------------------------------

    } // namespace model
} // namespace tomcat
