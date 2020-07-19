#pragma once

#include "NodeMetadata.h"
#include <iostream>

namespace tomcat {
    namespace model {

        /**
         * A node in a Dynamic Bayes Net (DBN). T must be the type of assignment
         * of the node.
         */
        template <typename T> class Node {
          public:
            Node() {}
            virtual ~Node() {}

            /**
             * Return the value currently assigned to the node.
             *
             * @return - node's assignment
             */
            virtual T get_assignment() const = 0;

            /**
             * Print a short description of the node.
             *
             * @param os: output stream
             */
            virtual void print(std::ostream& os) const {}

            friend std::ostream& operator<<(std::ostream& os,
                                            const Node<T>& node) {
                node.print(os);
                return os;
            };
        };

    } // namespace model
} // namespace tomcat