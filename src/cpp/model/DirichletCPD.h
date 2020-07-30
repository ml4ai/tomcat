#pragma once

#include "ContinuousCPD.h"
#include "Node.h"
#include <iostream>
#include <memory>

namespace tomcat {
    namespace model {

        /**
         * A Dirichlet CPD consists of a table containing \f$\alpha\f$: the
         * parameters of a Dirichlet (or Beta if the size of the vector
         * \f$\alpha\f$ is 2) distribution of the node that is sampled from this
         * CPD given its parents's assignments. The number of rows is given by
         * the product of the cardinalities of the parent nodes of such node.
         * Each row represents a combination of possible assignments of the
         * parent nodes ordered with respect to the binary basis.
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

        class DirichletCPD : public ContinuousCPD {
          private:
            int alpha_size;

            /**
             * Store the parameter table in the CPD
             *
             * @param parameter_table: vector of node shared pointers
             */
            void init_from_table(
                std::vector<std::shared_ptr<Node>>& parameter_table);

          protected:
            /**
             * Transform a table of numeric values for \f$\alpha\f$ to a list of
             * constant vector nodes to keep static and node dependent CPDs
             * compatible.
             *
             * @param matrix: matrix containing constant numerical
             * values for \f$\alpha\f$
             */
            void init_from_matrix(const Eigen::MatrixXd& matrix) override;

          public:
            /**
             * Store a list of node dependent parameters.
             *
             * @param parent_node_label_order: evaluation order of the parent
             * nodes assignment for correct table indexing
             * @param parameter_table: list of parameter vector \f$\alpha\f$
             * determined by other nodes' assignments
             */
            DirichletCPD(std::vector<std::string>& parent_node_label_order,
                         std::vector<std::shared_ptr<Node>>& parameter_table)
                : ContinuousCPD(parent_node_label_order) {

                this->init_from_table(parameter_table);
            }

            DirichletCPD(std::vector<std::string>&& parent_node_label_order,
                         std::vector<std::shared_ptr<Node>>&& parameter_table)
                : ContinuousCPD(std::move(parent_node_label_order)) {

                this->init_from_table(parameter_table);
            }

            /**
             * Create a dirichlet distribution from a matrix of parameter
             * values.
             *
             * @param parent_node_label_order: evaluation order of the parent
             * nodes assignment for correct table indexing
             * @param parameter_values: matrix containing constant numerical
             * values for \f$\alpha\f$
             */
            DirichletCPD(std::vector<std::string>& parent_node_label_order,
                         const Eigen::MatrixXd& parameter_values)
                : ContinuousCPD(parent_node_label_order) {

                this->init_from_matrix(parameter_values);
            }
            DirichletCPD(std::vector<std::string>&& parent_node_label_order,
                         const Eigen::MatrixXd&& parameter_values)
                : ContinuousCPD(std::move(parent_node_label_order)) {

                this->init_from_matrix(parameter_values);
            }

            ~DirichletCPD() {}

            DirichletCPD(const DirichletCPD& cpd) {
                this->copy_from_cpd(cpd);
                this->alpha_size = cpd.alpha_size;
            }
            DirichletCPD& operator=(const DirichletCPD& cpd) {
                this->copy_from_cpd(cpd);
                this->alpha_size = cpd.alpha_size;
                return *this;
            };

            DirichletCPD(DirichletCPD&& cpd) = default;
            DirichletCPD& operator=(DirichletCPD&& cpd) = default;

            /**
             * Sample a vector for each combination of parent nodes'
             * assignments (each row of the cpd table).
             *
             * @param random_generator: random number random_generator
             * @return matrix of sampled values. Each row contains a vector
             *  \f$\theta\f$ sampled from a dirichlet distribution with
             *  parameter vector \f$\alpha\f$ defined in each row of the
             *  parameter table.
             */
            Eigen::MatrixXd
            sample(std::shared_ptr<gsl_rng> random_generator) const override;

            Eigen::VectorXd sample(std::shared_ptr<gsl_rng> random_generator,
                                   int index) const override;

            void print(std::ostream& os) const override;

            std::unique_ptr<CPD> clone() const override;
        };

    } // namespace model
} // namespace tomcat
