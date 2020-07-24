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
            // Metadata is a shared pointer because each timed instance of a
            // node in the unrolled DBN will share the same metadata.
            std::shared_ptr<NodeMetadata> metadata;

            Eigen::VectorXd assignment;

          public:
            Node() {}
            Node(std::shared_ptr<NodeMetadata> metadata)
                : metadata(std::move(metadata))  {
            }
            Node(Eigen::VectorXd values)
                : assignment(std::move(values))  {
            }
            // Copy constructor and assignment should be deleted to avoid
            // implicit slicing and loss of polymorphic behaviour in the
            // subclasses. To deep copy, the clone method must be used.
            Node(const Node&) = delete;
            Node& operator=(const Node&) = delete;

            Node(Node&&) = default;
            Node& operator=(Node&&) = default;

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
            virtual void print(std::ostream& os) const = 0;

            /**
             * Return a short description of the node.
             *
             * @return Node's description
             */
            virtual std::string get_description() const = 0;

            /**
             * Clone node
             *
             * @return Pointer to the new node
             */
            virtual std::unique_ptr<Node> clone() const = 0;

            /**
             * Return node description. An instance of Node is considered a
             * constant node and, therefore, time agnostic.
             *
             * @return Node description
             */
            virtual std::string get_timed_name() const = 0;

            /**
             * Return node description for an arbitrary time step.
             *
             * @return Node's description for an arbitrary time step
             */
            virtual std::string get_timed_name(int time_step) const = 0;

            // --------------------------------------------------------
            // Getters
            // --------------------------------------------------------
            const std::shared_ptr<NodeMetadata>& get_metadata() const {
                return metadata;
            }

            const Eigen::VectorXd& get_assignment() const { return assignment; }
        };

    } // namespace model
} // namespace tomcat