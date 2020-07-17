#pragma once

#include "GaussianCPD.h"
#include <vector>
#include "Node.h"

namespace tomcat {
    namespace model {

        /*
         * A node dependent continuous CPD works similar to a continuous CPD but
         * the parameters of the distribution of a child node given its parents
         * is defined by the values sampled from another node. If some of the
         * parameters is fixed, a numerical node has to be passed.
         *
         * For instance,
         *
         * Let A and B be parents of the node C.
         *
         * A -> C B -> C
         *
         * Suppose A, B have cardinalities 2, 3 respectively and C
         * is sampled from a normal distribution with fixed \sigma^2 = 1. Let
         * N\mu_{ab} be a parameter node that contains the mean of such
         * distribution when A and B assume the value a and b respectively. Let
         * N1 be a numerical node with value 1 (instance of NumericalNode).
         *
         * A ContinuousCPD for C will be as follows,
         * _______________________________________
         * |///| C |     \mu     |    \sigma^1   |
         * |-------------------------------------|
         * | A | B |/////////////////////////////|
         * |-------------------------------------|
         * | 0 | 0 |  N\mu_{00}  |       N1      |
         * |-------------------------------------|
         * | 0 | 1 |  N\mu_{01}  |       N1      |
         * |-------------------------------------|
         * | 0 | 2 |  N\mu_{02}  |       N1      |
         * |-------------------------------------|
         * | 1 | 0 |  N\mu_{10}  |       N1      |
         * |-------------------------------------|
         * | 1 | 1 |  N\mu_{11}  |       N1      |
         * |-------------------------------------|
         * | 1 | 2 |  N\mu_{12}  |       N1      |
         * |-------------------------------------|
         *
         */

//        template <typename T>
//        class NodeDependentDiscreteCPD : public ContinuousCPD {
//
//            std::vector<std::vector<Node<T>>> parameter_dependency_table;
//        };

    } // namespace model
} // namespace tomcat
