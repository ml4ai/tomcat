#pragma once

#include "NodeMetadata.h"
#include <ostream>

namespace tomcat {
    namespace model {

        /*
         * A node in a Dynamic Bayes Net (DBN). The type T must encode the type
         * of value this node encodes, i.e., the type of a sample of this node.
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