#pragma once

#include "ContinuousCPD.h"
#include "Node.h"
#include <iostream>
#include <memory>

namespace tomcat {
    namespace model {

        enum PARAMETER_INDEX { mean, variance };

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
          private:
            /**
             * Store the parameter table in the CPD
             *
             * @param parameter_table: vector of gaussian parameters
             */
            void
            init_from_table(std::vector<GaussianParameters>& parameter_table);

          protected:
            /**
             * Transform a table of numeric values for mean and variance to
             * constant numeric nodes to keep static and node dependent CPDs
             * compatible.
             *
             * @param matrix: matrix containing constant numerical
             * values for mean and variance
             */
            virtual void
            init_from_matrix(const Eigen::MatrixXd& matrix) override;

          public:
            /**
             * Store a list of node dependent parameters.
             *
             * @param parent_node_label_order: evaluation order of the parent
             * nodes assignment for correct table indexing
             * @param parameter_table: list of gaussian parameters determined by
             * other nodes' assignments
             */
            GaussianCPD(std::vector<std::string>& parent_node_label_order,
                        std::vector<GaussianParameters>& parameter_table)
                : ContinuousCPD(parent_node_label_order) {

                this->init_from_table(parameter_table);
            }
            GaussianCPD(std::vector<std::string>&& parent_node_label_order,
                        std::vector<GaussianParameters>&& parameter_table)
                : ContinuousCPD(std::move(parent_node_label_order)) {

                this->init_from_table(parameter_table);
            }

            /**
             * Create a gaussian distribution from a matrix of parameter
             * values.
             *
             * @param parent_node_label_order: evaluation order of the parent
             * nodes assignment for correct table indexing
             * @param parameter_values: matrix containing constant numerical
             * values for mean and variance
             */
            GaussianCPD(std::vector<std::string>& parent_node_label_order,
                        Eigen::MatrixXd& parameter_values)
                : ContinuousCPD(parent_node_label_order) {

                this->init_from_matrix(parameter_values);
            }
            GaussianCPD(std::vector<std::string>&& parent_node_label_order,
                        Eigen::MatrixXd&& parameter_values)
                : ContinuousCPD(std::move(parent_node_label_order)) {

                this->init_from_matrix(parameter_values);
            }

            ~GaussianCPD() {}

            GaussianCPD(const GaussianCPD& cpd) { this->copy_from_cpd(cpd); }
            GaussianCPD& operator=(const GaussianCPD& cpd) {
                this->copy_from_cpd(cpd);
                return *this;
            };
            GaussianCPD(GaussianCPD&& cpd) = default;
            GaussianCPD& operator=(GaussianCPD&& cpd) = default;

            /**
             * Sample a numeric value for each combination of parent nodes'
             * assignments (each row of the cpd table).
             *
             * @param random_generator: random number random_generator
             * @return vector of sampled values. Each index contains a number
             *  sampled from gaussian distribution with mean and variance
             *  defined in each row of the parameter table.
             */
            Eigen::MatrixXd
            sample(std::shared_ptr<gsl_rng> random_generator) const override;

            Eigen::VectorXd
            sample(std::shared_ptr<gsl_rng> random_generator, int index) const override;

            /**
             * Print a short description of the distribution.
             *
             * @param os: output stream
             */
            void print(std::ostream& os) const override;

            /**
             * Clone CPD
             *
             * @return pointer to the new CPD
             */
            std::unique_ptr<CPD> clone() const override;
        };

    } // namespace model
} // namespace tomcat
