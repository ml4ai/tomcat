#pragma once

#include "ContinuousCPD.h"

namespace tomcat {
    namespace model {

        //------------------------------------------------------------------
        // Structs
        //------------------------------------------------------------------

        /**
         * Parameters of a univariate gaussian distribution.
         */
        struct GaussianParameters {
            std::shared_ptr<Node> mean;
            std::shared_ptr<Node> variance;

            GaussianParameters(std::shared_ptr<Node> mean,
                               std::shared_ptr<Node> variance)
                : mean(mean), variance(variance) {}

            GaussianParameters(std::shared_ptr<Node>&& mean,
                               std::shared_ptr<Node>&& variance)
                : mean(std::move(mean)), variance(std::move(variance)) {}
        };

        /**
         * A Gaussian CPD consists of a table containing the mean and variance
         * of a Gaussian distribution of the node that is sampled from this CPD
         * given its parents' assignments. The number of rows is given by the
         * product of the cardinalities of the parent nodes of such node. Each
         * row represents a combination of possible assignments of the parent
         * nodes ordered with respect to the binary basis.
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
        class GaussianCPD : public ContinuousCPD {
          public:
            //------------------------------------------------------------------
            // Types, Enums & Constants
            //------------------------------------------------------------------
            enum PARAMETER_INDEX { mean, variance };

            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates an instance of a Gaussian CPD comprised by a list of
             * node dependent parameters.
             *
             * @param parent_node_label_order: evaluation order of the parent
             * nodes' assignments for correct table indexing
             * @param parameter_table: list of two parameter nodes which
             * assignments determine the two parameters of the distribution:
             * mean and variance.
             */
            GaussianCPD(std::vector<std::string>& parent_node_label_order,
                        std::vector<GaussianParameters>& parameter_table);

            /**
             * Creates an instance of a Gaussian CPD comprised by a list of
             * node dependent parameters.
             *
             * @param parent_node_label_order: evaluation order of the parent
             * nodes' assignments for correct table indexing
             * @param parameter_table: list of two parameter nodes which
             * assignments determine the two parameters of the distribution:
             * mean and variance.
             */
            GaussianCPD(std::vector<std::string>&& parent_node_label_order,
                        std::vector<GaussianParameters>&& parameter_table);

            /**
             * Creates an instance of a Gaussian CPD table by transforming a
             * table of parameter values to a list of constant vector nodes to
             * keep static and node dependent CPDs compatible.
             *
             * @param parent_node_label_order: evaluation order of the parent
             * nodes' assignments for correct table indexing
             * @param cpd_table: 2-columns matrix containing constant numerical
             * values for mean and variance
             */
            GaussianCPD(std::vector<std::string>& parent_node_label_order,
                        Eigen::MatrixXd& parameter_values);

            /**
             * Creates an instance of a Gaussian CPD table by transforming a
             * table of parameter values to a list of constant vector nodes to
             * keep static and node dependent CPDs compatible.
             *
             * @param parent_node_label_order: evaluation order of the parent
             * nodes' assignments for correct table indexing
             * @param cpd_table: 2-columns matrix containing constant numerical
             * values for mean and variance
             */
            GaussianCPD(std::vector<std::string>&& parent_node_label_order,
                        Eigen::MatrixXd&& parameter_values);

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

          protected:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Samples a value from a Gaussian distribution comprised by the
             * mean and variance assigned to the nodes in the a specific row of
             * the parameter table.
             *
             * @param random_generator: random number generator
             * @param table_row: row of the parameter table containing the
             * nodes that store the mean and the variance of the distribution
             * @return Value sampled from a Gaussian distribution
             */
            virtual Eigen::VectorXd
            sample_from_table_row(std::shared_ptr<gsl_rng> random_generator,
                                  int table_row) const override;

            /**
             * Transforms a table of numeric values for mean and variance to
             * constant numeric nodes to keep static and node dependent CPDs
             * compatible.
             *
             * @param matrix: matrix containing constant numerical
             * values for mean and variance
             */
            virtual void
            init_from_matrix(const Eigen::MatrixXd& matrix) override;

          private:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Stores the parameter table in the CPD. It transforms the mean and
             * variance from a Gaussian parameter struct into a list of nodes
             * and adds it to the parameter table to comply with the format of
             * this data member.
             *
             * @param parameter_table: vector of gaussian parameters
             */
            void
            init_from_table(std::vector<GaussianParameters>& parameter_table);
        };

    } // namespace model
} // namespace tomcat
