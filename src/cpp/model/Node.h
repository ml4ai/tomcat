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
             * @param value: node's constant assignment
             */
            Node(double value);

            /**
             * Create a constant node with a multidimensional value assigned.
             *
             * @param values: node's constant assignment
             */
            Node(Eigen::VectorXd values)
                : assignment(std::move(values)) {}
            virtual ~Node() {}

            // Copy constructor and assignment should be deleted to avoid
            // implicit slicing and loss of polymorphic behaviour in the
            // subclasses. To deep copy, the clone method must be used.
            Node(const Node&) = default;
            Node& operator=(const Node&) = default;

            Node(Node&&) = default;
            Node& operator=(Node&&) = default;


            /**
             * Move constructor
             *
             * @param node: node to be moved
             */
            //Node(Node&& node) : assignment(std::move(node.assignment)) {}

            friend std::ostream& operator<<(std::ostream& os,
                                            const Node& node) {
                node.print(os);
                return os;
            };

            /**
             * Print a short description of the node.
             *
             * @param os: output stream
             */
            virtual void print(std::ostream& os) const;

            /**
             * Clone node
             *
             * @return pointer to the new node
             */
            virtual std::unique_ptr<Node> clone() const;

            // Getters
            const Eigen::VectorXd& get_assignment() const { return assignment; }

            virtual void test() {
                std::cout << "Test Base";
            };

        };

    } // namespace model
} // namespace tomcat