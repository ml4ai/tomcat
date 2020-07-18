#pragma once

#include "Node.h"
#include <iostream>
#include <vector>
#include <eigen3/Eigen/Dense>

namespace tomcat {
    namespace model {

        class ConstantVectorNode : public Node<Eigen::VectorXd> {
          private:
            Eigen::VectorXd values;

          public:
            ConstantVectorNode(Eigen::VectorXd values)
                : values(std::move(values)) {}
            ~ConstantVectorNode() {}

            Eigen::VectorXd get_assignment() const override;

            void print(std::ostream& os) const override;
        };

    } // namespace model
} // namespace tomcat