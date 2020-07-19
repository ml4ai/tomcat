#pragma once

#include "CPD.h"
#include "Node.h"
#include <iostream>
#include <memory>

namespace tomcat {
    namespace model {

        /**
         * A categorical CPD consists of a table containing the probabilities
         * p(column | row). The number of rows is given by the product of the
         * cardinalities of the parent nodes of the node that is sampled from
         * this CPD. Each row represents a combination of possible assignments
         * of the parent nodes ordered in ascending order with respect to the
         * binary basis. A categorical CPD is used for discrete probabilities.
         *
         * For instance,
         *
         * Let A and B be parents of the node C.
         *
         * A -> C B -> C
         *
         * Suppose A, B and C have cardinalities 2, 3 and 4 respectively.
         *
         * Let p(c|a,b) be p(C = c | A = a, B = b). A CategoricalCPD for C will
         * be as follows,
         * _____________________________________________________
         * |///| C |     0    |     1    |     2    |     3    |
         * |---------------------------------------------------|
         * | A | B |///////////////////////////////////////////|
         * |---------------------------------------------------|
         * | 0 | 0 | p(0|0,0) | p(1|0,0) | p(2|0,0) | p(3|0,0) |
         * |---------------------------------------------------|
         * | 0 | 1 | p(0|0,1) | p(1|0,1) | p(2|0,1) | p(3|0,1) |
         * |---------------------------------------------------|
         * | 0 | 2 | p(0|0,2) | p(1|0,2) | p(2|0,2) | p(3|0,2) |
         * |---------------------------------------------------|
         * | 1 | 0 | p(0|1,0) | p(1|1,0) | p(2|1,0) | p(3|1,0) |
         * |---------------------------------------------------|
         * | 1 | 1 | p(0|1,1) | p(1|1,1) | p(2|1,1) | p(3|1,1) |
         * |---------------------------------------------------|
         * | 1 | 2 | p(0|1,2) | p(1|1,2) | p(2|1,2) | p(3|1,2) |
         * |---------------------------------------------------|
         */
        class CategoricalCPD : public CPD {
          private:
            std::vector<std::unique_ptr<Node<Eigen::VectorXd>>>
                probability_table;

          public:
            /**
             * Store a list of node dependent discrete distributions.
             *
             * @param parent_node_label_order: evaluation order of the parent
             * nodes assignment for correct table indexing
             * @param cpd_table: list of probabilities determined other by
             * nodes' assignments
             */
            CategoricalCPD(
                std::vector<std::string> parent_node_label_order,
                std::vector<std::unique_ptr<Node<Eigen::VectorXd>>> cpd_table)
                : CPD(std::move(parent_node_label_order)),
                  probability_table(std::move(cpd_table)) {}

            /**
             * Transform a table of probabilities to a list of constant vector
             * nodes to keep static and node dependent CPDs compatible.
             *
             * @param parent_node_label_order: evaluation order of the parent
             * nodes assignment for correct table indexing
             * @param cpd_table: matrix containing constant numerical
             * probabilities
             */
            CategoricalCPD(std::vector<std::string> parent_node_label_order,
                           Eigen::MatrixXd& cpd_table);

            /**
             * Move constructor. There's no copy constructor because the cpd
             * table is internally stored as a vector of unique pointers as
             * CPD's specific distributions won't be shared among nodes.
             *
             * @param cpd: categorical CPD for copy
             */
            CategoricalCPD(CategoricalCPD&& cpd)
                : CPD(std::move(cpd.parent_node_label_order)),
                  probability_table(std::move(cpd.probability_table)) {}

            ~CategoricalCPD() {}

            /**
             * Sample a numeric value for each combination of parent nodes'
             * assignments (each row of the cpd table).
             *
             * @param generator: random number generator
             * @return vector of sampled values. Each index contains a number
             *  sampled from a categorical distribution defined by each row of
             *  the cpd table.
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
