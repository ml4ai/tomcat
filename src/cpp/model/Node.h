#pragma once

#include "NodeMetadata.h"
#include <iostream>
#include <type_traits>

namespace tomcat {
    namespace model {

        /**
         * A node in a Dynamic Bayes Net (DBN). T must be the type of assignment
         * of the node.
         */
        template <typename T> class Node {
          protected:
            T assignment;

          public:
            Node() {}
            Node(T assignment) : assignment(std::move(assignment)) {}
            virtual ~Node() {}

            /**
             * Print a short description of the node.
             *
             * @param os: output stream
             */
            virtual void print(std::ostream& os) const {
                // This is not a good design but this differentiation between
                // types is only needed for log purposes so far. If there are
                // other specialized behaviours between the different types in
                // the future, subclasses will be created and this method
                // implemented in each one of them.
                if constexpr (std::is_same<T, Eigen::VectorXd>::value) {
                    os << "Node(["
                       << static_cast<Eigen::VectorXd>(this->assignment)
                              .transpose()
                       << "])";
                }
                else {
                    os << "Node(" << this->assignment << ")";
                }
            }

            friend std::ostream& operator<<(std::ostream& os,
                                            const Node<T>& node) {
                node.print(os);
                return os;
            };

            // Getters
            const T& get_assignment() const { return assignment; }
        };

    } // namespace model
} // namespace tomcat