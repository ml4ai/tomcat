#pragma once

#include <array>
#include <memory>
#include <vector>

#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/subgraph.hpp>

#include "../../utils/Definitions.h"
#include "../cpd/CPD.h"
#include "ExactInferenceNode.h"

namespace tomcat {
    namespace model {

        /**
         * Class description here
         */
        class FactorGraph {
          public:
            //------------------------------------------------------------------
            // Structs
            //------------------------------------------------------------------
            struct VertexData {
                std::shared_ptr<ExactInferenceNode> node;

                // Indicates whether the node is a factor node.
                bool factor;

                int time_step;
            };

            //------------------------------------------------------------------
            // Types, Enums & Constants
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            FactorGraph();

            ~FactorGraph();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------

            // Copy constructor and assignment should be deleted to avoid
            // implicit slicing and loss of polymorphic behaviour in the
            // subclasses. To deep copy, the clone method must be used.
            FactorGraph(const FactorGraph&) = delete;

            FactorGraph& operator=(const FactorGraph&) = delete;

            FactorGraph(FactorGraph&&) = default;

            FactorGraph& operator=(FactorGraph&&) = default;

            //------------------------------------------------------------------
            // Operator overload
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Static functions
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------
            void add_node(const std::string& node_label,
                          int cardinality,
                          int time_step,
                          Eigen::MatrixXd& cpd,
                          const CPD::TableOrderingMap& cpd_ordering_map);

            void add_edge(const std::string& source_node_label,
                          int source_node_time_step,
                          const std::string& target_node_label,
                          int target_node_time_step);

            /**
             * Computes the order of the nodes to visit in topological order for
             * time slice 0, 1 and 2 individually and store this order for later
             * retrieval.
             */
            void compute_topological_traversal_per_time_slice();

            std::vector<VertexData> get_vertices_topological_order(
                int time_step, bool from_roots_to_leaves = true) const;

            std::vector<VertexData> get_parents_of(const ExactInferenceNode::NodeName& node_name) const;

            std::vector<VertexData>
            get_children_of(const ExactInferenceNode::NodeName& node_name) const;

            Eigen::VectorXd get_marginal_for(const ExactInferenceNode::NodeName& node_name) const;

            //------------------------------------------------------------------
            // Virtual functions
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Pure virtual functions
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Getters & Setters
            //------------------------------------------------------------------

          protected:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------

          private:
            //------------------------------------------------------------------
            // Types, Enums & Constants
            //------------------------------------------------------------------

            // The graph is defined as bidirectional to speed up the access to
            // the list of parents and children of a vertex. However, only
            // single-direction edges will be created in reality. Also, a factor
            // graph is defined as an undirected graph. However, since the
            // potential functions are actually CPDs, to make the computation of
            // the messages more straightforward, we keep the direction of the
            // edges in the original DBN.
            typedef boost::subgraph<boost::adjacency_list<boost::vecS,
                                                          boost::vecS,
                                                          boost::bidirectionalS,
                                                          VertexData>>
                Graph;

            typedef std::unordered_map<std::string, int> IDMap;

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------
            std::string
            get_factor_label(const std::string& target_non_factor_label) const;

            std::string get_relative_name_for(const ExactInferenceNode::NodeName& node_name) const;

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------
            std::unique_ptr<Graph> graph;

            IDMap name_to_id;

            // Last time step represented by concrete nodes in the factor
            // graph. A DBN can have a different structure up to the second time
            // step in this implementation. For instance, in time step zero it's
            // common to have priors which do not show up in the following time
            // steps. Also, there could be the case that some node points to
            // another in time step 1. The third time step (t = 2) would then be
            // comprised by a structure of nodes that do not change over time
            // anymore. So inference methods can loop over the structure of the
            // time step 2 without the need to create concrete nodes. We allow
            // the last_time_step to be less than 2 if the DBN is actually a
            // single Bayes Net (only one time step) as this implementation is
            // flexible enough to allow working with them. In that case,
            // max_time_step would be 0.
            int last_time_step;

            // Vertices that are part of the sub-graph comprised by the nodes in
            // time step 0, 1 and 2. This information will be useful to compute
            // the sub-graphs per time step and subsequently a topological order
            // over them.
            std::array<std::vector<int>, 3> time_sliced_vertices;

            // The two members below store the topological orders for each one
            // of the time slices sub-graphs. A reversed topological order here
            // is defined as a traversal from the leaves to the roots.
            std::array<std::vector<VertexData>, 3>
                time_sliced_topological_order;

            std::array<std::vector<VertexData>, 3>
                time_sliced_reversed_topological_order;
        };

    } // namespace model
} // namespace tomcat
