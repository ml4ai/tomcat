#pragma once

#include "CPD.h"
#include "Node.h"
#include "NodeMetadata.h"
#include <memory>

namespace tomcat {
    namespace model {

        /**
         * A random variable node is a concrete node in the unrolled DBN
         * that has a distribution from which it can be sampled from. The
         * assignment of this node can change as we sample from it's posterior
         * distribution over the other nodes' assignments in the unrolled DBN.
         */
        template <typename T> class RandomVariableNode : public Node<T> {
          protected:
            // Metadata is a shared pointer because each timed instance of a
            // node in the unrolled DBN will share the same metadata.
            std::shared_ptr<NodeMetadata> metadata;

            // CPD is a unique pointer because it's an abstract class and each
            // node should have it's own CPD even if they are instances
            // generated from the same metadata. This is necessary because a CPD
            // of a node can depend on other nodes' assignment.
            std::unique_ptr<CPD> cpd;

            int time_step;
            T assignment;

          public:
            /**
             * Creates a node instance in an unrolled DBN
             *
             * @param metadata: node's metadata
             * @param cpd: node's conditional probability distribution
             * @param time_step: node's time step in the unrolled DBN
             */
            RandomVariableNode(std::shared_ptr<NodeMetadata> metadata,
                               std::unique_ptr<CPD> cpd,
                               int time_step = 0)
                : metadata(std::move(metadata)), cpd(std::move(cpd)),
                  time_step(time_step) {}
            virtual ~RandomVariableNode() {}

            /**
             * Return the node's assignment.
             *
             * @return value currently assigned to the node
             */
            T get_assignment() const { return this->assignment; };

            /**
             * Print a short description of the node.
             *
             * @param os: output stream
             */
            void print(std::ostream& os) const {
                // This is not a good design but this differentiation between
                // types is only needed for log purposes so far. If there are
                // other specialized behaviours between the different types in
                // the future, subclasses will be created and this method
                // implemented in each one of them.
                if constexpr (std::is_same<T, Eigen::VectorXd>::value) {
                    os << "RV(" << this->metadata->label;
                    os << ", ";
                    os << this->time_step;
                    os << ", [";
                    os << static_cast<Eigen::VectorXd>(this->assignment)
                           .transpose();
                    os << "])";
                }
                else {
                    os << "RV(" << this->metadata->label;
                    os << ", ";
                    os << this->time_step;
                    os << ", ";
                    os << this->assignment;
                    os << ")";
                }
            };

            // Getters
            const std::shared_ptr<NodeMetadata>& get_metadata() const {
                return metadata;
            }

            const std::unique_ptr<CPD>& get_cpd() const { return cpd; }

            int get_time_step() const { return time_step; }

            // Setters
            void set_assignment(T assignment) {
                this->assignment = std::move(assignment);
            }
        };

    } // namespace model
} // namespace tomcat