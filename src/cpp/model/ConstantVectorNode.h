#pragma once

#include "Node.h"
#include <eigen3/Eigen/Dense>
#include <iostream>
#include <vector>

namespace tomcat {
    namespace model {

        /**
         * A constant vector node is a deterministic node, i.e., this node's
         * assignment is a constant vector of numeric values. Constant vector
         * nodes are useful to fix parameters of a node dependent CPD from a
         * continuous distribution or probabilities of a categorical
         * distribution.
         */
        class ConstantVectorNode : public Node<Eigen::VectorXd> {
          private:
            Eigen::VectorXd values;

          public:
            /**
             * Assign a vector of numeric values to the node.
             *
             * @param values: vector of numeric values assigned to the node
             */
            ConstantVectorNode(Eigen::VectorXd values)
                : values(std::move(values)) {}
            ~ConstantVectorNode() {}

            /**
             * Return the node's assignment.
             *
             * @return vector of constant numeric values assigned to node
             */
            Eigen::VectorXd get_assignment() const override;

            /**
             * Print a short description of the node.
             *
             * @param os: output stream
             */
            void print(std::ostream& os) const override;
        };

    } // namespace model
} // namespace tomcat