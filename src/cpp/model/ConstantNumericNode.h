#pragma once

#include "Node.h"
#include <iostream>

namespace tomcat {
    namespace model {

        /*
         * A constant node is a deterministic node, i.e., a sample from this
         * node is a constant value. Constant nodes are useful to specify
         * constant parameters or fixed distributions in a node dependent CPD.
         */
        class ConstantNumericNode : public Node<double> {
          private:
            double value;

          public:
            ConstantNumericNode(double value) : value(value) {}
            ~ConstantNumericNode() {}

            double get_assignment() const override;

            void print(std::ostream& os) const override;
        };

    } // namespace model
} // namespace tomcat