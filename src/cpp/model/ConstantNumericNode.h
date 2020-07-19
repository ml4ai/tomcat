#pragma once

#include "Node.h"
#include <iostream>

namespace tomcat {
    namespace model {

        /**
         * A constant node is a deterministic node, i.e., this node's assignment
         * is a constant value. Constant numeric nodes are useful to fix
         * parameters of a node dependent CPD from a continuous distribution.
         */
        class ConstantNumericNode : public Node<double> {
          private:
            double value;

          public:
            /**
             * Assign a numeric value to the node.
             *
             * @param value: numeric value assigned to the node
             */
            ConstantNumericNode(double value) : value(value) {}
            ~ConstantNumericNode() {}

            /**
             * Return the node's assignment.
             *
             * @return constant numeric value assigned to the node
             */
            double get_assignment() const override;

            /**
             * Print a short description of the node.
             *
             * @param os: output stream
             */
            void print(std::ostream& os) const override;
        };

    } // namespace model
} // namespace tomcat