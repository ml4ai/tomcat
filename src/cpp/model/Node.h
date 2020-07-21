#pragma once

#include "NodeMetadata.h"
#include <eigen3/Eigen/Dense>
#include <iostream>

namespace tomcat {
    namespace model {

        /**
         * A node in a Dynamic Bayes Net (DBN).
         */
        class Node {
          protected:
            Eigen::VectorXd assignment;

          public:
            Node() {}

            /**
             * Create a constant node with a numerical value assigned.
             *
             * @param value - node's constant assignment
             */
            Node(double value);

            /**
             * Create a constant node with a multidimensional value assigned.
             *
             * @param values - node's constant assignment
             */
            Node(Eigen::VectorXd values)
                : assignment(std::move(values)) {}
            virtual ~Node() {}

            /**
             * Copy constructor
             *
             * @param node: node to be copied
             */
            Node(const Node& node) : assignment(node.assignment) {}

            /**
             * Move constructor
             *
             * @param node: node to be moved
             */
            Node(Node&& node) : assignment(std::move(node.assignment)) {}

            /**
             * Print a short description of the node.
             *
             * @param os: output stream
             */
            virtual void print(std::ostream& os) const;

            friend std::ostream& operator<<(std::ostream& os,
                                            const Node& node) {
                node.print(os);
                return os;
            };

            // Getters
            const Eigen::VectorXd& get_assignment() const { return assignment; }

        };

    } // namespace model
} // namespace tomcat