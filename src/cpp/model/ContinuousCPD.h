#pragma once

#include "CPD.h"
#include <Eigen/Dense>

namespace tomcat {
    namespace model {

        /*
         * A continuous CPD consists of a table containing the parameters of the
         * distribution of the node that is sampled from this CPD given parent
         * assignments. The number of rows is given by the product of the
         * cardinalities of the parent nodes of such node. Each row represents a
         * combination of possible assignments of the parent nodes ordered in
         * ascending order in a binary basis.
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
         * A ContinuousCPD for C will be as follows,
         * ______________________________________
         * |///| C |    \mu   |    \sigma^2     |
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
//        class ContinuousCPD : public CPD {
//          private:
//            Eigen::MatrixXd parameter_table;
//        };

    } // namespace model
} // namespace tomcat
