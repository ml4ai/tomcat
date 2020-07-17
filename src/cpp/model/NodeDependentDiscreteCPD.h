#pragma once

#include "DiscreteCPD.h"
#include "Node.h"

namespace tomcat {
    namespace model {

        /*
         * A node dependent discrete CPD works similar to a discrete CPD but the
         * distribution of a child node given its parents is defined by the
         * values sampled from another node.
         *
         * For instance,
         *
         * Let A and B be parents of the node C.
         *
         * A -> C B -> C
         *
         * Let \theta_{ab} be a parameter node that contains the discrete
         * probabilities of C when A and B assume the value a and b
         * respectively.
         *
         * Suppose A, B and C have cardinalities 2, 3 and 3 respectively, and
         * \theta_{ab} is sampled from a Dirichlet distribution with 3
         * parameters.
         *
         * A DiscreteCPD for C will be as follows,
         * _______________________________________
         * |///| C |    0    |    1    |    2    |
         * |-------------------------------------|
         * | A | B |/////////////////////////////|
         * |-------------------------------------|
         * | 0 | 0 |   Categorical(\theta_{00})  |
         * |-------------------------------------|
         * | 0 | 1 |   Categorical(\theta_{01})  |
         * |-------------------------------------|
         * | 0 | 2 |   Categorical(\theta_{02})  |
         * |-------------------------------------|
         * | 1 | 0 |   Categorical(\theta_{10})  |
         * |-------------------------------------|
         * | 1 | 1 |   Categorical(\theta_{11})  |
         * |-------------------------------------|
         * | 1 | 2 |   Categorical(\theta_{12})  |
         * |-------------------------------------|
         *
         */

//        template <typename T>
//        class NodeDependentDiscreteCPD : public DiscreteCPD {
//
//            std::vector<Node<T>> probability_dependency_table;
//
//        };

    } // namespace model
} // namespace tomcat
