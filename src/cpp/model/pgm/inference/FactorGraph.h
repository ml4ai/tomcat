#pragma once

#include <array>
#include <functional>
#include <memory>
#include <string>
#include <unordered_set>
#include <utility>
#include <vector>

#include <boost/graph/adjacency_list.hpp>

#include "pgm/DynamicBayesNet.h"
#include "pgm/inference/FactorNode.h"
#include "pgm/inference/MessageNode.h"
#include "utils/Definitions.h"

namespace tomcat {
    namespace model {

        /**
         * A factor graph is comprised of factor nodes and variable nodes and is
         * used to compute marginalized distributions via exact inference with
         * message passing algorithms. In this representation of the factor
         * graph, nodes can be added up to time step 2, being the nodes and
         * connections in the last time step assigned (repeatable time step)
         * used as templates for message passing to future time steps. The nodes
         * in the repeatable time step will store messages starting in this time
         * step and beyond in a map. This solves the problem of having to unroll
         * the graph, creating instances of nodes in specific future time steps
         * to compute and store messages. Since the structure in the repeatable
         * time step repeats indefinitely, the messages in future time steps can
         * be processed in a loop over that structure. Since this graph does not
         * contain concrete nodes for every time step, the instances of nodes in
         * this graph are denominated template nodes.
         */
        class FactorGraph {
          public:
            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates an empty factor graph.
             */
            FactorGraph();

            ~FactorGraph();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------
            FactorGraph(const FactorGraph&) = default;

            FactorGraph& operator=(const FactorGraph&) = default;

            FactorGraph(FactorGraph&&) = default;

            FactorGraph& operator=(FactorGraph&&) = default;

            //------------------------------------------------------------------
            // Static functions
            //------------------------------------------------------------------

            /**
             * Creates a factor graph from a unrolled DBN by adding the nodes
             * and connections up to time step 2 from the DBN structure.
             *
             * @param dbn: unrolled dynamic Bayes net
             *
             * @return Factor graph.
             */
            static FactorGraph
            create_from_unrolled_dbn(const DynamicBayesNet& dbn);

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Adds a variable node and a parent factor node attached to it to
             * the factor graph.
             *
             * @param node_label: variable node's label
             * @param cardinality: cardinality of the variable node
             * @param time_step: time step where the nodes (variable and its
             * factor) should be added (up to 2)
             * @param cpd: cpd of the variable node given its parents. This will
             * be converted into a potential function stored in the factor node
             * created in this function.
             * @param cpd_ordering_map: how parent nodes index the cpd table
             */
            void add_node(const std::string& node_label,
                          int cardinality,
                          int time_step,
                          const Eigen::MatrixXd& cpd,
                          const CPD::TableOrderingMap& cpd_ordering_map);

            /**
             * Adds a link between a variable node and the parent factor node of
             * another variable node.
             *
             * @param source_node_label: source variable node's label
             * @param source_node_time_step: source variable node's time step
             * @param target_node_label: target variable node's label
             * @param target_node_time_step: target variable node's time step
             */
            void add_edge(const std::string& source_node_label,
                          int source_node_time_step,
                          const std::string& target_node_label,
                          int target_node_time_step);

            /**
             * Stores nodes int topological order (forward and backwards) for
             * individual time steps up to the template time step. Some
             * algorithms may require to process messages in a single time step
             * before moving to the next one. By storing the traversal order
             * beforehand, computational speed is reduced.
             */
            void store_topological_traversal_per_time_step();

            /**
             * Returns the vertices in topological order in the sub-graph
             * defined by the nodes in a specific time step.
             *
             * @param time_step: time step of the sub-graph
             * @param from_roots_to_leaves: whether the traversal is done in a
             * top-down fashion
             *
             * @return Nodes in topological order within a time step
             */
            std::vector<std::shared_ptr<MessageNode>>
            get_vertices_topological_order_in(
                int time_step, bool from_roots_to_leaves = true) const;

            /**
             * Returns a list of pairs (parent, transition) for a given template
             * node where parent is one of the node's parents and transition
             * indicates whether that parent comes from a previous time step or
             * not. The latter is needed for when the retrieving parents for
             * nodes in a time step greater than the repeatable time step of the
             * graph, because the template parent node will have the same time
             * step of the child node (they both come from the repeatable
             * structure since this structure repeats beyond that
             * point in time).
             *
             * @param node: node in the factor graph
             * @param time_step: real time step
             *
             * @return Parents of the template node informed
             */
            std::vector<std::pair<std::shared_ptr<MessageNode>, bool>>
            get_parents_of(const std::shared_ptr<MessageNode>& template_node,
                           int time_step) const;

            /**
             * Returns the children of a given node. The child nodes are not
             * constrained to the node's time step and can come from a posterior
             * time step.
             *
             * @param node: node in the factor graph
             *
             * @return Children of the template node informed
             */
            std::vector<std::shared_ptr<MessageNode>> get_children_of(
                const std::shared_ptr<MessageNode>& template_node) const;

            /**
             * Returns the marginal distribution for a given node in a certain
             * point in time. The marginal is given by the multiplication of all
             * the incoming messages to the node.
             *
             * @param node_label: node's label
             * @param time_step: time step of the node
             * @param normalized: whether the multiplication of incoming
             * messages must be normalized or not.
             *
             * @return Marginal distribution
             */
            Eigen::MatrixXd get_marginal_for(const std::string& node_label,
                                             int time_step,
                                             bool normalized) const;

            /**
             * Clears messages and beyond a given time step (not inclusive).
             *
             * @param time_step: time step
             */
            void erase_incoming_messages_beyond(int time_step);

            /**
             * Returns factor nodes that are connected to nodes in a given time
             * step and the previous one. Transition factor nodes from the repeatable time step are return
             * whenever the given time step is greater than the repeatable one.
             *
             * @param time_step: factor nodes' time step
             *
             * @return Transition factor nodes in time step.
             */
            std::unordered_set<std::shared_ptr<FactorNode>>
            get_transition_factors_at(int time_step) const;

          private:
            //------------------------------------------------------------------
            // Types, Enums & Constants
            //------------------------------------------------------------------

            // The graph is defined as bidirectional to speed up the access to
            // the list of parents and children of a vertex. However, only
            // single-direction edges will be created in reality. Also, a factor
            // graph is theoretically defined as an undirected graph. However,
            // since the potential functions are actually CPDs, to make the
            // computation of the messages more straightforward, the directions
            // of the edges in the original DBN are preserved.
            typedef boost::adjacency_list<boost::vecS,
                                          boost::vecS,
                                          boost::bidirectionalS,
                                          std::shared_ptr<MessageNode>>
                Graph;

            typedef std::unordered_map<std::string, int> IDMap;

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Adds a template variable node to the graph.
             *
             * @param node_label: node's label
             * @param cardinality: node's cardinality
             * @param time_step: node's time step (up to 2)
             *
             * @return Index of the vertex in the graph.
             */
            int add_variable_node(const std::string& node_label,
                                  int cardinality,
                                  int time_step);

            /**
             * Adds a template factor node to the graphh.
             *
             * @param node_label: node's label
             * @param time_step: node's time step (up to 2)
             * @param cpd: cpd table of the factor node's child
             * @param cpd_ordering_map: how the factor's parent nodes index its
             * cpd table
             *
             * @return Index of the vertex in the graph.
             */
            int add_factor_node(const std::string& node_label,
                                int time_step,
                                const Eigen::MatrixXd& cpd,
                                const CPD::TableOrderingMap& cpd_ordering_map);

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------
            Graph graph;

            IDMap name_to_id;

            // Last time step represented by template nodes in the factor graph.
            // A DBN can have a different structure up to the second time step
            // in this implementation. For instance, in time step zero it's
            // common to have priors which do not show up in the following time
            // steps. Also, there could be the case that some node points to
            // another in time step 1. The third time step (t = 2) would then be
            // comprised by a structure of nodes that do not change over time
            // anymore. So inference methods can loop over the structure of the
            // time step 2 without the need to create concrete nodes. The
            // repeatable time step is allowed to be less than 2 if the DBN is
            // actually a single Bayes Net (only one time step) as this
            // implementation is flexible enough to allow working with this kind
            // of PGM. In that case, max_time_step would be 0.
            int repeatable_time_step = 0;

            // The two data structures below store the topological orders for
            // each one of the time-step sub-graphs. A reversed topological
            // order here is defined as a traversal from the leaves to the
            // roots.
            std::array<std::vector<std::shared_ptr<MessageNode>>, 3>
                time_sliced_topological_order;

            std::array<std::vector<std::shared_ptr<MessageNode>>, 3>
                time_sliced_reversed_topological_order;

            // Stores the factor nodes that link variable nodes in two different
            // time steps.

            std::array<std::unordered_set<std::shared_ptr<FactorNode>>, 3>
                transition_factors_per_time_step;
        };

    } // namespace model
} // namespace tomcat
