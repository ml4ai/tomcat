#pragma once

#include "CPD.h"
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

        class DirichletCPD : public CPD {
          private:
            std::vector<std::unique_ptr<Node<Eigen::VectorXd>>> parameter_table;

          public:
            /**
             * Store a list of node dependent parameters.
             *
             * @param parent_node_label_order: evaluation order of the parent
             * nodes assignment for correct table indexing
             * @param parameter_table: list of parameter vector \f$\alpha\f$
             * determined by other nodes' assignments
             */
            DirichletCPD(std::vector<std::string> parent_node_label_order,
                         std::vector<std::unique_ptr<Node<Eigen::VectorXd>>>
                             parameter_table)
                : CPD(std::move(parent_node_label_order)),
                  parameter_table(std::move(parameter_table)) {}

            /**
             * Transform a table of numeric values for \f$\alpha\f$ to a list of
             * constant vector nodes to keep static and node dependent CPDs
             * compatible.
             *
             * @param parent_node_label_order: evaluation order of the parent
             * nodes assignment for correct table indexing
             * @param parameter_table: matrix containing constant numerical
             * values for \f$\alpha\f$
             */
            DirichletCPD(std::vector<std::string> parent_node_label_order,
                         Eigen::MatrixXd& parameter_table);

            /**
             * Move constructor. There's no copy constructor because the cpd
             * table is internally stored as a vector of unique pointers as
             * CPD's specific distributions won't be shared among nodes.
             *
             * @param cpd: dirichlet CPD for copy
             */
            DirichletCPD(DirichletCPD&& cpd)
                : CPD(std::move(cpd.parent_node_label_order)),
                  parameter_table(std::move(cpd.parameter_table)) {}

            ~DirichletCPD() {}

            /**
             * Sample a vector for each combination of parent nodes' assignments
             * (each row of the cpd table).
             *
             * @param generator: random number generator
             * @return matrix of sampled values. Each row contains a vector
             *  \f$\theta\f$ sampled from a dirichlet distribution with
             *  parameter vector \f$\alpha\f$ defined in each row of the
             *  parameter table.
             */
            Eigen::MatrixXd
            sample(std::shared_ptr<gsl_rng> generator) const override;

            /**
             * Print a short description of the distribution.
             *
             * @param os: output stream
             */
            void print(std::ostream& os) const override;
        };

    } // namespace model
} // namespace tomcat
