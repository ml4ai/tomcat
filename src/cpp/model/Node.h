#pragma once

#include "NodeMetadata.h"
#include <iostream>

namespace tomcat {
    namespace model {

        /*
         * A node in a Dynamic Bayes Net (DBN). T must be the type of a sample
         * from this node.
         */
        template <typename T> class Node {
          public:
            Node() {}

            virtual ~Node() {}

            /*
             * Sample from the node's distribution.
             */
            virtual T sample() const = 0;

            virtual void print(std::ostream& os) const {}

            friend std::ostream& operator<<(std::ostream& os,
                                            const Node<T>& node) {
                node.print(os);
                return os;
            };
        };

    } // namespace model
} // namespace tomcat