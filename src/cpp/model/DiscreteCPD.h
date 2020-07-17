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
         * A discrete CPD consists of a table containing the probabilities
         * p(column | row). The number of rows is given by the product of the
         * cardinalities of the parent nodes of the node that is sampled from
         * this CPD. Each row represents a combination of possible assignments
         * of the parent nodes ordered in ascending order with respect to the
         * binary basis.
         *
         * For instance,
         *
         * Let A and B be parents of the node C.
         *
         * A -> C B -> C
         *
         * Suppose A, B and C have cardinalities 2, 3 and 4 respectively.
         *
         * Let p(c|a,b) be p(C = c | A = a, B = b). A DiscreteCPD for C will be
         * as follows,
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
        class DiscreteCPD : public CPD<double> {
          private:
            std::vector<std::unique_ptr<Node<std::vector<double>>>> cpd_table;

          public:
            DiscreteCPD(std::vector<std::string> parent_node_label_order,
                        std::vector<std::unique_ptr<Node<std::vector<double>>>>
                            cpd_table)
                : CPD<double>(std::move(parent_node_label_order)),
                  cpd_table(std::move(cpd_table)) {}

            /*
             * Transform a table of probabilities to a list of constant vector
             * nodes to keep static and node dependent CPDs compatible.
             */
            DiscreteCPD(std::vector<std::string> parent_node_label_order,
                        Eigen::MatrixXd& cpd_table);

            // There's no copy constructor because the table is formed by a
            // vector of unique pointers. There will not be a shared CPD anyway.
            DiscreteCPD(DiscreteCPD&& cpd)
                : CPD<double>(std::move(cpd.parent_node_label_order)),
                  cpd_table(std::move(cpd.cpd_table)) {}
            ~DiscreteCPD() {}

            double sample() const override;

            void print(std::ostream& os) const override;
        };

    } // namespace model
} // namespace tomcat
