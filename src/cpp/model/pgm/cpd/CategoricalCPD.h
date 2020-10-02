#pragma once

#include "utils/Definitions.h"
#include "distribution/Categorical.h"
#include "pgm/cpd/CPD.h"

namespace tomcat {
    namespace model {

        /**
         * A categorical CPD consists of a table containing the probabilities
         * p(column | row). The number of rows is given by the product of the
         * cardinalities of the parent nodes of the node that is sampled from
         * this CPD. Each row represents a combination of possible assignments
         * of the parent nodes ordered in ascending order with respect to the
         * binary basis. A categorical CPD is used for discrete probabilities.
         * The table is represented by a list of categorical distributions that
         * contain a list of probabilities in itself.
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
             * Creates an instance of a Categorical CPD.
             *
             * @param parent_node_order: evaluation order of the parent
             * nodes' assignments for correct distribution indexing
             * @param distributions: list of categorical distributions
             */
            CategoricalCPD(
                std::vector<std::shared_ptr<NodeMetadata>>& parent_node_order,
                const std::vector<std::shared_ptr<Categorical>>& distributions);

            /**
             * Creates an instance of a Categorical CPD.
             *
             * @param parent_node_order: evaluation order of the parent
             * nodes' assignments for correct distribution indexing
             * @param distributions: list of categorical distributions
             */
            CategoricalCPD(
                std::vector<std::shared_ptr<NodeMetadata>>&& parent_node_order,
                const std::vector<std::shared_ptr<Categorical>>& distributions);

            /**
             * Creates an instance of a Categorical CPD by transforming a
             * table of probabilities to a list of categorical distributions
             * with constant probabilities.
             *
             * @param parent_node_order: evaluation order of the parent
             * nodes' assignments for correct distribution indexing
             * @param probabilities: matrix containing probabilities
             */
            CategoricalCPD(
                std::vector<std::shared_ptr<NodeMetadata>>& parent_node_order,
                const Eigen::MatrixXd& probabilities);

            /**
             * Creates an instance of a Categorical CPD by transforming a
             * table of probabilities to a list of categorical distributions
             * with constant probabilities.
             *
             * @param parent_node_order: evaluation order of the parent
             * nodes' assignments for correct distribution indexing
             * @param probabilities: matrix containing probabilities
             */
            CategoricalCPD(
                std::vector<std::shared_ptr<NodeMetadata>>&& parent_node_order,
                const Eigen::MatrixXd& probabilities);

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
            std::unique_ptr<CPD> clone() const override;

            std::string get_description() const override;

            void add_to_sufficient_statistics(
                const Eigen::VectorXd& sample) override;

            Eigen::MatrixXd sample_from_conjugacy(
                std::shared_ptr<gsl_rng> random_generator,
                const std::vector<std::shared_ptr<Node>>& parent_nodes,
                int num_samples) const override;

            void reset_sufficient_statistics() override;

          protected:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------
            void clone_distributions() override;

          private:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Initialized the CPD from a list of distributions.
             *
             * @param distributions: list of categorical distributions.
             */
            void init_from_distributions(
                const std::vector<std::shared_ptr<Categorical>>& distributions);

            /**
             * Uses the values in the matrix to create a list of constant
             * categorical distributions.
             *
             * @param matrix: matrix of probabilities
             */
            void init_from_matrix(const Eigen::MatrixXd& matrix);

        };

    } // namespace model
} // namespace tomcat
