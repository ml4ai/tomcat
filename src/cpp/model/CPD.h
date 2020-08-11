#pragma once

#include <memory>
#include <vector>

#include <eigen3/Eigen/Dense>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_rng.h>

#include "Node.h"

namespace tomcat {
    namespace model {

        //------------------------------------------------------------------
        // Structs
        //------------------------------------------------------------------

        /** This struct represents stores indexing information for fast
         * retrieval of a distribution in the CPD table given parent nodes'
         * assignments.
         */
        struct ParentIndexing {

            // Order of the parent node's label for table indexing
            int order;

            // Cummulative cardinality of the nodes to the right of the parent
            // node's label border.
            int right_cumulative_cardinality;
        };

        /**
         * Abstract representation of a conditional probability distribution.
         *
         * In a CPD table, each row consists of the distribution of a child node
         * given its parents' assignments. It can also be comprised of a single
         * row if the CPD defines a prior (not conditioned in any parent node's
         * assignment). For the discrete case, each row contains the
         * probabilities that define a categorical distribution. For the
         * continuous case, in turn, each row has the parameters of the
         * underlying continuous distribution.
         */
        class CPD {
          public:
            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates an abstract representation of a conditional probability
             * distribution.
             */
            CPD();

            /**
             * Creates an abstract representation of a conditional probability
             * distribution.
             *
             * @param parent_node_order: evaluation order of the parent
             * nodes' assignments for correct table indexing
             */
            CPD(std::vector<std::shared_ptr<NodeMetadata>>& parent_node_order);

            /**
             * Creates an abstract representation of a conditional probability
             * distribution.
             *
             * @param parent_node_label_order: evaluation order of the parent
             * nodes' assignments for correct table indexing
             */
            CPD(std::vector<std::shared_ptr<NodeMetadata>>&& parent_node_order);

            virtual ~CPD();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------

            // Copy constructor and assignment should be deleted to avoid
            // implicit slicing and loss of polymorphic behaviour in the
            // subclasses. To deep copy, the clone method must be used.
            CPD(const CPD&) = delete;

            CPD& operator=(const CPD&) = delete;

            CPD(CPD&& cpd) = default;

            CPD& operator=(CPD&& cpd) = default;

            //------------------------------------------------------------------
            // Operator overload
            //------------------------------------------------------------------
            friend std::ostream& operator<<(std::ostream& os, const CPD& cpd);

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Draws a sample from a distribution in a given row of the CPD
             * table given parent node's assignments.
             *
             * @param random_generator: random number random_generator
             * @param parent_labels_to_nodes: mapping between a node's label and
             * its concrete object representation in an unrolled DBN.
             *
             * @return A sample from one of the distributions in the CPD table.
             */
            Eigen::VectorXd
            sample(std::shared_ptr<gsl_rng> random_generator,
                   const Node::NodeMap& parent_labels_to_nodes) const;

            /**
             * Draws a sample from a distribution (weighted by children's
             * assignments) in a given row of the CPD table given parent node's
             * assignments.
             *
             * @param random_generator: random number random_generator
             * @param parent_labels_to_nodes: mapping between a node's label and
             * its concrete object representation in an unrolled DBN.
             * @param child_nodes: children of node that owns the CPD
             *
             * @return A sample from one of the distributions (weighted) in the
             * CPD table.
             */
            // TODO - remove
            //            Eigen::VectorXd sample_weighted(
            //                std::shared_ptr<gsl_rng> random_generator,
            //                const Node::NodeMap& parent_labels_to_nodes,
            //                const
            //                std::vector<std::shared_ptr<RandomVariableNode>>&
            //                    child_nodes) const;

            /**
             * Marks the CPD as not updated to force dependency updating on a
             * subsequent call to the member function update_dependencies.
             */
            void reset_updated_status();

            /**
             * Prints a short description of the distribution.
             *
             * @param os: output stream
             */
            void print(std::ostream& os) const;

            //------------------------------------------------------------------
            // Pure virtual functions
            //------------------------------------------------------------------
            /**
             * Replaces parameter nodes in a node dependent CPD by the correct
             * replica of the node in an unrolled DBN. When a CPD is defined, a
             * random variable node can be assigned as one of the distributions
             * of the table. As a value is assigned to this node, it serves as
             * parameters of the underlying distribution of the CPD.
             *
             * @param parameter_nodes_map: mapping between a parameter node's
             * timed name and its concrete object representation in an unrolled
             * DBN.
             * @param time_step: time step of the node that owns the CPD in the
             * unrolled DBN. It can be different from the time step of the
             * parameter node if the latter is shared among nodes over several
             * time steps.
             */
            virtual void update_dependencies(Node::NodeMap& parameter_nodes_map,
                                             int time_step) = 0;

            /**
             * Creates a new unique pointer from a concrete instance of a CPD.
             *
             * @return Pointer to the new CPD.
             */
            virtual std::unique_ptr<CPD> clone() const = 0;

            //------------------------------------------------------------------
            // Getters & Setters
            //------------------------------------------------------------------
            const std::string& get_id() const;

            bool is_updated() const;

            const std::unordered_map<std::string, ParentIndexing>&
            get_parent_label_to_indexing() const;

          protected:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Copies data members of a generic CPD.
             *
             * @param cpd: generic CPD
             */
            void copy_from_cpd(const CPD& cpd);

            //------------------------------------------------------------------
            // Pure virtual functions
            //------------------------------------------------------------------

            /**
             * Samples a value from a CPD table row comprised by the parameters
             * of the distribution associated with it.
             *
             * @param random_generator: random number generator
             * @param table_row: row of the CPD table containing the
             * node that stores the distribution's parameters
             * @return A sample from the distribution defined by the parameters
             * in the CPD table row.
             */
            virtual Eigen::VectorXd
            sample_from_table_row(std::shared_ptr<gsl_rng> random_generator,
                                  int table_row) const = 0;

            /**
             * Returns a short description of the CPD.
             *
             * @return CPD's description.
             */
            virtual std::string get_description() const = 0;

            /**
             * Clones the nodes the CPD depends on.
             */
            virtual void clone_distributions() = 0;

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------

            // Unique identifier formed by the concatenation of the parent
            // labels in alphabetical order delimited by comma.
            std::string id;

            // It defines the order of the parent nodes in the cartesian
            // product of their possible assignments. It's necessary to know
            // this order for correctly index a distribution given parent's
            // assignments.
            std::vector<std::shared_ptr<NodeMetadata>> parent_node_order;

            // It indicates whether the CPD was updated with concrete instances
            // of the nodes it depends on
            bool updated = false;

          private:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Fill CPD's unique identifier formed by the concatenation of the parent
            // labels in alphabetical order delimited by comma.
             */
            void init_id();

            /**
             * Fills mapping table for quick access to a distribution in the CPD
             * table given parent node's assignments.
             */
            void fill_indexing_mapping();

            /**
             * Returns the index of the distribution (row in the table) given
             * parents' assignments.
             *
             * A CPD is comprised of a table where each row represents a certain
             * distribution. The number of rows is given by the size of the
             * cartesian product of parent nodes' cardinalities, and each row
             * represents a combination of the values these parent nodes can
             * assume.
             *
             * @param parent_labels_to_nodes: Mapping between parent node's
             * labels and node objects.
             * @return Index of distribution assigned to specific parent nodes'
             * assignments.
             */
            int get_table_row_given_parents_assignments(
                const Node::NodeMap& parent_labels_to_nodes) const;

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------
            std::unordered_map<std::string, ParentIndexing>
                parent_label_to_indexing;
        };

    } // namespace model
} // namespace tomcat
