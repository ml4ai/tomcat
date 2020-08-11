#pragma once

#include "CPD.h"
#include "Categorical.h"

namespace tomcat {
    namespace model {

        /**
         * A categorical CPD consists of a table containing the probabilities
         * p(column | row). The number of rows is given by the product of the
         * cardinalities of the parent nodes of the node that is sampled from
         * this CPD. Each row represents a combination of possible assignments
         * of the parent nodes ordered in ascending order with respect to the
         * binary basis. A categorical CPD is used for discrete probabilities.
         *
         * For instance,
         *
         * Let A and B be parents of the node C.
         *
         * A -> C B -> C
         *
         * Suppose A, B and C have cardinalities 2, 3 and 4 respectively.
         *
         * Let p(C = c | A = a, B = b) be p(c|a,b). A CategoricalCPD for C will
         * be as follows,
         * _____________________________________________________
         * |///| C |     0    |     1    |     2    |     3    |
         * |---------------------------------------------------|
         * | A | B |///////////////////////////////////////////|
         * |---------------------------------------------------|
         * | 0 | 0 | p(0|0,0) | p(1|0,0) | p(2|0,0) | p(3|0,0) |
         * |---------------------------------------------------|
         * | 0 | 1 | p(0|0,1) | p(1|0,1) | p(2|0,1) | p(3|0,1) |
         * |---------------------------------------------------|
         * | 0 | 2 | p(0|0,2) | p(1|0,2) | p(2|0,2) | p(3|0,2) |
         * |---------------------------------------------------|
         * | 1 | 0 | p(0|1,0) | p(1|1,0) | p(2|1,0) | p(3|1,0) |
         * |---------------------------------------------------|
         * | 1 | 1 | p(0|1,1) | p(1|1,1) | p(2|1,1) | p(3|1,1) |
         * |---------------------------------------------------|
         * | 1 | 2 | p(0|1,2) | p(1|1,2) | p(2|1,2) | p(3|1,2) |
         * |---------------------------------------------------|
         */
        class CategoricalCPD : public CPD {
          public:
            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates an instance of a Categorical CPD table comprised of a
             * list of node dependent probabilities.
             *
             * @param parent_node_order: evaluation order of the parent
             * nodes' assignments for correct table indexing
             * @param cpd_table: table of probabilities determined other by
             * nodes' assignments
             */
            CategoricalCPD(
                std::vector<std::shared_ptr<NodeMetadata>>& parent_node_order,
                std::vector<std::shared_ptr<Categorical>>& cpd_table);

            /**
             * Creates an instance of a Categorical CPD table comprised of a
             * list of node dependent probabilities.
             *
             * @param parent_node_order: evaluation order of the parent
             * nodes' assignments for correct table indexing
             * @param cpd_table: table of probabilities determined other by
             * nodes' assignments
             */
            CategoricalCPD(
                std::vector<std::shared_ptr<NodeMetadata>>&& parent_node_order,
                std::vector<std::shared_ptr<Categorical>>&& cpd_table);

            /**
             * Creates an instance of a Categorical CPD table by transforming a
             * table of probabilities to a list of constant vector nodes to keep
             * static and node dependent CPDs compatible.
             *
             * @param parent_node_order: evaluation order of the parent
             * nodes' assignments for correct table indexing
             * @param cpd_table: matrix containing constant numerical
             * probabilities
             */
            CategoricalCPD(
                std::vector<std::shared_ptr<NodeMetadata>>& parent_node_order,
                const Eigen::MatrixXd& cpd_table);

            /**
             * Creates an instance of a Categorical CPD table by transforming a
             * table of probabilities to a list of constant vector nodes to keep
             * static and node dependent CPDs compatible.
             *
             * @param parent_node_order: evaluation order of the parent
             * nodes assignment for correct table indexing
             * @param cpd_table: matrix containing constant numerical
             * probabilities
             */
            CategoricalCPD(
                std::vector<std::shared_ptr<NodeMetadata>>&& parent_node_order,
                const Eigen::MatrixXd&& cpd_table);

            ~CategoricalCPD();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------
            CategoricalCPD(const CategoricalCPD& cpd);

            CategoricalCPD& operator=(const CategoricalCPD& cpd);

            CategoricalCPD(CategoricalCPD&& cpd) = default;

            CategoricalCPD& operator=(CategoricalCPD&& cpd) = default;

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------
            virtual void update_dependencies(Node::NodeMap& parameter_nodes_map,
                                             int time_step) override;

            std::unique_ptr<CPD> clone() const override;

            std::string get_description() const override;

          protected:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Samples a value from a categorical distribution comprised by the
             * probabilities assigned one of the nodes in the probability table.
             *
             * @param random_generator: random number generator
             * @param table_row: row of the probability table containing the
             * node that stores the list of discrete probabilities
             * @return Value sampled from a categorical distribution
             */
            virtual Eigen::VectorXd
            sample_from_table_row(std::shared_ptr<gsl_rng> random_generator,
                                  int table_row) const override;

            void clone_distributions() override;

          private:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Fills the probability table with constant nodes created from a
             * matrix of constant numerical values.
             *
             * @param matrix: matrix of probabilities
             */
            void init_from_matrix(const Eigen::MatrixXd& matrix);

            /**
             * Copies data members of a categorical CPD.
             *
             * @param cpd: categorical CPD
             */
            void copy_from_cpd(const CategoricalCPD& cpd);

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------
            std::vector<std::shared_ptr<Categorical>> probability_table;
        };

    } // namespace model
} // namespace tomcat
