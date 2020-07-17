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
         * A Gaussian CPD consists of a table containing the mean and variance
         * of a Gaussian distribution of the node that is sampled from this CPD
         * given its parent assignments. The number of rows is given by the
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
         * from a normal distribution with mean \mu and variance \sigma^2.
         *
         * A GaussianCPD for C will be as follows,
         * ______________________________________
         * |///| C |   $\mu$  |   $\sigma^2$    |
         * |------------------------------------|
         * | A | B |////////////////////////////|
         * |------------------------------------|
         * | 0 | 0 | $\mu_00$ | $\sigma^2_{00}$ |
         * |------------------------------------|
         * | 0 | 1 | $\mu_01$ | $\sigma^2_{01}$ |
         * |------------------------------------|
         * | 0 | 2 | $\mu_02$ | $\sigma^2_{02}$ |
         * |------------------------------------|
         * | 1 | 0 | $\mu_10$ | $\sigma^2_{10}$ |
         * |------------------------------------|
         * | 1 | 1 | $\mu_11$ | $\sigma^2_{11}$ |
         * |------------------------------------|
         * | 1 | 2 | $\mu_12$ | $\sigma^2_{12}$ |
         * |------------------------------------|
         */

        struct GaussianParameters {
            std::unique_ptr<Node<double>> mean;
            std::unique_ptr<Node<double>> variance;

            GaussianParameters(std::unique_ptr<Node<double>> mean,
                              std::unique_ptr<Node<double>> variance)
                : mean(std::move(mean)), variance(std::move(variance)) {}
        };

        class GaussianCPD : public CPD<Eigen::MatrixXd> {
          private:
            std::vector<GaussianParameters> parameter_table;

          public:
            GaussianCPD(std::vector<std::string> parent_node_label_order,
                        std::vector<GaussianParameters> parameter_table)
                : CPD<Eigen::MatrixXd>(std::move(parent_node_label_order)),
                  parameter_table(std::move(parameter_table)) {}

            /*
             * Transform a table of parameter values to a list of constant
             * gaussian parameters comprised of constant nodes to keep static
             * and node dependent CPDs compatible.
             */
            GaussianCPD(std::vector<std::string> parent_node_label_order,
                        Eigen::MatrixXd& parameter_table);

            // There's no copy constructor because the table is formed by a
            // struct which components are unique pointers. There will not be a
            // shared CPD anyway.
            GaussianCPD(GaussianCPD&& cpd)
                : CPD<Eigen::MatrixXd>(std::move(cpd.parent_node_label_order)),
                  parameter_table(std::move(cpd.parameter_table)) {}

            ~GaussianCPD() {}

            Eigen::MatrixXd sample() const override;

            void print(std::ostream& os) const override;
        };

    } // namespace model
} // namespace tomcat
