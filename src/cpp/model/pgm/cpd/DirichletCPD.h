#pragma once

#include "model/utils/Definitions.h"
#include "model/distribution/Dirichlet.h"
#include "model/pgm/cpd/CPD.h"

namespace tomcat {
    namespace model {

        /**
         * A Dirichlet CPD consists of a table containing \f$\alpha\f$: the
         * parameters of a Dirichlet (or Beta if the size of the vector
         * \f$\alpha\f$ is 2) distribution of the node that is sampled from this
         * CPD given its parents' assignments. The number of rows is given by
         * the product of the cardinalities of these parent nodes. Each row
         * represents a combination of possible assignments of the parent nodes
         * ordered with respect to the binary basis. The table is represented by
         * a list of Dirichlet distributions that contain a list of coefficients
         * of \f$\alpha\f$ in itself.
         *
         * For instance,
         *
         * Let A and B be parents of the node C.
         *
         * A -> C B -> C
         *
         * Suppose A, B have cardinalities 2, 3 respectively and C is sampled
         * from a dirichlet distribution with parameter vector \f$\alpha\f$ of
         * size 3.
         *
         * A DirichletCPD for C will be as follows,
         * ______________________________________
         * |///| C |       \f$\alpha\f$         |
         * |------------------------------------|
         * | A | B |////////////////////////////|
         * |------------------------------------|
         * | 0 | 0 |     \f$\alpha_{00}\f$      |
         * |------------------------------------|
         * | 0 | 1 |     \f$\alpha_{01}\f$      |
         * |------------------------------------|
         * | 0 | 2 |     \f$\alpha_{02}\f$      |
         * |------------------------------------|
         * | 1 | 0 |     \f$\alpha_{10}\f$      |
         * |------------------------------------|
         * | 1 | 1 |     \f$\alpha_{11}\f$      |
         * |------------------------------------|
         * | 1 | 2 |     \f$\alpha_{12}\f$      |
         * |------------------------------------|
         */

        class DirichletCPD : public CPD {
          public:
            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates an instance of a Dirichlet CPD.
             *
             * @param parent_node_order: evaluation order of the parent
             * nodes' assignments for correct distribution indexing
             * @param distributions: list of Dirichlet distributions
             */
            DirichletCPD(
                std::vector<std::shared_ptr<NodeMetadata>>& parent_node_order,
                const std::vector<std::shared_ptr<Dirichlet>>& distributions);

            /**
             * Creates an instance of a Dirichlet CPD.
             *
             * @param parent_node_order: evaluation order of the parent
             * nodes' assignments for correct distribution indexing
             * @param distributions: list of Dirichlet distributions
             */
            DirichletCPD(
                std::vector<std::shared_ptr<NodeMetadata>>&& parent_node_order,
                const std::vector<std::shared_ptr<Dirichlet>>& distributions);

            /**
             * Creates an instance of a Dirichlet CPD table by transforming a
             * table of parameter values to a list of Dirichlet distributions
             * with constant parameters.
             *
             * @param parent_node_order: evaluation order of the parent
             * nodes' assignments for correct distribution indexing
             * @param cpd_table: matrix containing a \f$\alpha\f$s
             */
            DirichletCPD(
                std::vector<std::shared_ptr<NodeMetadata>>& parent_node_order,
                const Eigen::MatrixXd& alphas);

            /**
             * Creates an instance of a Dirichlet CPD table by transforming a
             * table of parameter values to a list of Dirichlet distributions
             * with constant parameters.
             *
             * @param parent_node_order: evaluation order of the parent
             * nodes' assignments for correct distribution indexing
             * @param cpd_table: matrix containing a \f$\alpha\f$s
             */
            DirichletCPD(
                std::vector<std::shared_ptr<NodeMetadata>>&& parent_node_order,
                const Eigen::MatrixXd& alphas);

            ~DirichletCPD();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------
            DirichletCPD(const DirichletCPD& cpd);

            DirichletCPD& operator=(const DirichletCPD& cpd);

            DirichletCPD(DirichletCPD&& cpd) = default;

            DirichletCPD& operator=(DirichletCPD&& cpd) = default;

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
             * @param distributions: list of Dirichlet distributions.
             */
            void init_from_distributions(
                const std::vector<std::shared_ptr<Dirichlet>>& distributions);

            /**
             * Uses the values in the matrix to create a list of constant
             * Dirichlet distributions.
             *
             * @param matrix: matrix of \f$\alpha\f$s
             */
            void init_from_matrix(const Eigen::MatrixXd& matrix);

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------

            /**
             * Sufficient statistics used for CPDs owned by parameter nodes.
             * It's used to compute the posterior of a conjugate prior.
             */
            Eigen::VectorXd sufficient_statistics;
        };

    } // namespace model
} // namespace tomcat
