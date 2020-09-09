#pragma once

#include "../../utils/Definitions.h"
#include "../../distribution/Gaussian.h"
#include "CPD.h"

namespace tomcat {
    namespace model {

        /**
         * A Gaussian CPD consists of a table containing the mean and variance
         * of a Gaussian distribution of the node that is sampled from this CPD
         * given its parents' assignments. The number of rows is given by the
         * product of the cardinalities of the parent nodes of such node. Each
         * row represents a combination of possible assignments of the parent
         * nodes ordered with respect to the binary basis. The table is
         * represented by a list of Gaussian distributions that contain the mean
         * and the variance in itself.
         *
         * For instance,
         *
         * Let A and B be parents of the node C.
         *
         * A -> C B -> C
         *
         * Suppose A, B have cardinalities 2, 3 respectively and C is sampled
         * from a normal distribution with mean \f$\mu\f$ and variance
         * \f$\sigma^2\f$.
         *
         * A GaussianCPD for C will be as follows,
         * ______________________________________________
         * |///| C |   \f$\mu\f$  |   \f$\sigma^2\f$    |
         * |--------------------------------------------|
         * | A | B |////////////////////////////////////|
         * |--------------------------------------------|
         * | 0 | 0 | \f$\mu_00\f$ | \f$\sigma^2_{00}\f$ |
         * |--------------------------------------------|
         * | 0 | 1 | \f$\mu_01\f$ | \f$\sigma^2_{01}\f$ |
         * |--------------------------------------------|
         * | 0 | 2 | \f$\mu_02\f$ | \f$\sigma^2_{02}\f$ |
         * |--------------------------------------------|
         * | 1 | 0 | \f$\mu_10\f$ | \f$\sigma^2_{10}\f$ |
         * |--------------------------------------------|
         * | 1 | 1 | \f$\mu_11\f$ | \f$\sigma^2_{11}\f$ |
         * |--------------------------------------------|
         * | 1 | 2 | \f$\mu_12\f$ | \f$\sigma^2_{12}\f$ |
         * |--------------------------------------------|
         */
        class GaussianCPD : public CPD {
          public:
            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates an instance of a Gaussian CPD.
             *
             * @param parent_node_order: evaluation order of the parent
             * nodes' assignments for correct distribution indexing
             * @param distributions: list of Gaussian distributions
             */
            GaussianCPD(
                std::vector<std::shared_ptr<NodeMetadata>>& parent_node_order,
                std::vector<std::shared_ptr<Gaussian>>& distributions);

            /**
             * Creates an instance of a Gaussian CPD.
             *
             * @param parent_node_order: evaluation order of the parent
             * nodes' assignments for correct distribution indexing
             * @param distributions: list of Gaussian distributions
             */
            GaussianCPD(
                std::vector<std::shared_ptr<NodeMetadata>>&& parent_node_order,
                std::vector<std::shared_ptr<Gaussian>>&& distributions);

            /**
             * Creates an instance of a Gaussian CPD table by transforming a
             * table of parameter values to a list of Gaussian distributions
             * with constant mean and variance.
             *
             * @param parent_node_order: evaluation order of the parent
             * nodes' assignments for correct distribution indexing
             * @param cpd_table: matrix containing the means and variances
             */
            GaussianCPD(
                std::vector<std::shared_ptr<NodeMetadata>>& parent_node_order,
                const Eigen::MatrixXd& parameters);

            /**
             * Creates an instance of a Gaussian CPD table by transforming a
             * table of parameter values to a list of Gaussian distributions
             * with constant mean and variance.
             *
             * @param parent_node_order: evaluation order of the parent
             * nodes' assignments for correct distribution indexing
             * @param cpd_table: matrix containing the means and variances
             */
            GaussianCPD(
                std::vector<std::shared_ptr<NodeMetadata>>&& parent_node_order,
                const Eigen::MatrixXd& parameters);

            ~GaussianCPD();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------
            GaussianCPD(const GaussianCPD& cpd);

            GaussianCPD& operator=(const GaussianCPD& cpd);

            GaussianCPD(GaussianCPD&& cpd) = default;

            GaussianCPD& operator=(GaussianCPD&& cpd) = default;

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
             * Uses the values in the matrix to create a list of constant
             * Dirichlet distributions.
             *
             * @param matrix: matrix of \f$\alpha\f$s
             */
            virtual void init_from_matrix(const Eigen::MatrixXd& matrix);
        };

    } // namespace model
} // namespace tomcat
