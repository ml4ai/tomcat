#pragma once

#include "CPD.h"
#include "Node.h"
#include <eigen3/Eigen/Dense>
#include <iostream>
#include <memory>
#include <vector>

namespace tomcat {
    namespace model {

        /*
         * A Dirichlet CPD consists of a table containing \alpha parameters of a
         * Dirichlet (or Beta if the size of the vector \alpha is 2)
         * distribution of the node that is sampled from this CPD given its
         * parent assignments. The number of rows is given by the product of the
         * cardinalities of the parent nodes of such node. Each row represents a
         * combination of possible assignments of the parent nodes ordered with
         * respect to the binary basis.
         *
         * For instance,
         *
         * Let A and B be parents of the node C.
         *
         * A -> C B -> C
         *
         * Suppose A, B have cardinalities 2, 3 respectively and C is sampled
         * from a dirichlet distribution with parameter vector \alpha of size 3.
         *
         * A DirichletCPD for C will be as follows,
         * ______________________________________
         * |///| C |         $\alpha$           |
         * |------------------------------------|
         * | A | B |////////////////////////////|
         * |------------------------------------|
         * | 0 | 0 |       $\alpha_{00}$        |
         * |------------------------------------|
         * | 0 | 1 |       $\alpha_{01}$        |
         * |------------------------------------|
         * | 0 | 2 |       $\alpha_{02}$        |
         * |------------------------------------|
         * | 1 | 0 |       $\alpha_{10}$        |
         * |------------------------------------|
         * | 1 | 1 |       $\alpha_{11}$        |
         * |------------------------------------|
         * | 1 | 2 |       $\alpha_{12}$        |
         * |------------------------------------|
         */

        class DirichletCPD : public CPD<Eigen::MatrixXd> {
          private:
            std::vector<std::unique_ptr<Node<Eigen::VectorXd>>> parameter_table;

          public:
            DirichletCPD(std::vector<std::string> parent_node_label_order,
                         std::vector<std::unique_ptr<Node<Eigen::VectorXd>>>
                             parameter_table)
                : CPD<Eigen::MatrixXd>(std::move(parent_node_label_order)),
                  parameter_table(std::move(parameter_table)) {}

            /*
             * Transform a table of parameter values to a list of constant
             * vector nodes to keep static and node dependent CPDs compatible.
             */
            DirichletCPD(std::vector<std::string> parent_node_label_order,
                         Eigen::MatrixXd& parameter_table);

            // There's no copy constructor because the table is formed by a
            // vector of unique pointers. There will not be a shared CPD anyway.
            DirichletCPD(DirichletCPD&& cpd)
                : CPD<Eigen::MatrixXd>(std::move(cpd.parent_node_label_order)),
                  parameter_table(std::move(cpd.parameter_table)) {}

            ~DirichletCPD() {}

            Eigen::MatrixXd sample(std::shared_ptr<gsl_rng> generator) const override;

            void print(std::ostream& os) const override;
        };

    } // namespace model
} // namespace tomcat
