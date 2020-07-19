#pragma once

#include "CategoricalCPD.h"
#include "Node.h"
#include "NodeMetadata.h"
#include <memory>

namespace tomcat {
    namespace model {

        /**
         * A random variable numeric node is a concrete node in the unrolled DBN
         * that has a distribution from which it can be sampled from. The
         * assignment of this node can change as we sample from it's posterior
         * distribution over the other nodes' assignments in the unrolled DBN.
         */
        class RandomVariableNumericNode : public Node<double> {
          private:
            // Metadata is a shared pointer because each timed instance of a
            // node in the unrolled DBN will share the same metadata.
            std::shared_ptr<NodeMetadata> metadata;

            // CPD is a unique pointer because it's an abstract class and each
            // node should have it's own CPD even if they are instances
            // generated from the same metadata. This is necessary because a CPD
            // of a node can depend on other nodes' assignment.
            std::unique_ptr<CPD> cpd;

            int time_step;
            double assignment;

          public:
            /**
             * Creates a node instance in an unrolled DBN
             *
             * @param metadata: node's metadata
             * @param cpd: node's conditional probability distribution
             * @param time_step: node's time step in the unrolled DBN
             */
            RandomVariableNumericNode(std::shared_ptr<NodeMetadata> metadata,
                                      std::unique_ptr<CPD> cpd,
                                      int time_step = 0)
                : metadata(std::move(metadata)), cpd(std::move(cpd)),
                  time_step(time_step) {}
            ~RandomVariableNumericNode() {}

            /**
             * Return the node's assignment.
             *
             * @return numeric value currently assigned to the node
             */
            double get_assignment() const override;

            /**
             * Print a short description of the node.
             *
             * @param os: output stream
             */
            void print(std::ostream& os) const override;

            // Getters
            const std::shared_ptr<NodeMetadata>& get_metadata() const {
                return metadata;
            }

            const std::unique_ptr<CPD>& get_cpd() const {
                return cpd;
            }

            int get_time_step() const { return time_step; }

            // Setters
            void set_assignment(double assignment) {
                RandomVariableNumericNode::assignment = assignment;
            }
        };

    } // namespace model
} // namespace tomcat