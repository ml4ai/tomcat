#pragma once

#include "Node.h"
#include <iostream>
#include <vector>

namespace tomcat {
    namespace model {

        class ConstantVectorNode : public Node<std::vector<double>> {
          private:
            std::vector<double> values;

          public:
            ConstantVectorNode(std::vector<double> values)
                : values(std::move(values)) {}
            ~ConstantVectorNode() {}

            /*
             * Return the vector of numeric values stored in this node
             */
            std::vector<double> sample() const override;

            void print(std::ostream& os) const override;
        };

    } // namespace model
} // namespace tomcat