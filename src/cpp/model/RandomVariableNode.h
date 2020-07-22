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
        class RandomVariableNode : public Node {
          protected:
            // Metadata is a shared pointer because each timed instance of a
            // node in the unrolled DBN will share the same metadata.
            std::shared_ptr<NodeMetadata> metadata;

            // CPD is a unique pointer because it's an abstract class and each
            // node should have it's own CPD even if they are instances
            // generated from the same metadata. This is necessary because a CPD
            // of a node can depend on other nodes' assignment.
            std::unique_ptr<CPD> cpd;

            // Time step where the node is drawn in the unrolled DBN. This
            // variable will be assigned when this node is created as a vertex
            // in an urolled DBN.
            int time_step;

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

            ~RandomVariableNode() {}

            RandomVariableNode(const RandomVariableNode& node)
                : metadata(node.metadata), cpd(node.cpd->clone()),
                  time_step(node.time_step), Node(node) {}
            RandomVariableNode& operator=(const RandomVariableNode& node) {
                this->metadata = node.metadata;
                this->cpd = node.cpd->clone();
                this->time_step = node.time_step;
                this->assignment = node.assignment;
                return *this;
            }
            RandomVariableNode(RandomVariableNode&&) = default;
            RandomVariableNode& operator=(RandomVariableNode&&) = default;

            /**
             * Print a short description of the node.
             *
             * @param os: output stream
             */
            void print(std::ostream& os) const override;

            /**
             * Return a short description of the node.
             */
            std::string get_description() const override;

            /**
             * Set assignment from a single numeric value if samples from this
             * node is 1-dimensional.
             *
             * @param assignment: numeric value
             */
            void set_assignment(double assignment);

            /**
             * Clone node
             *
             * @return pointer to the new node
             */
            std::unique_ptr<Node> clone() const override;

            /**
             * Return the list of parent nodes that are parameter nodes.
             *
             * @return parameter nodes
             */
            std::vector<std::shared_ptr<RandomVariableNode>>
            get_parameter_parents() const;

            /**
             * Return the node name formed by it's label and time_step in the
             * unrolled DBN.
             *
             * @return node name in the unrolled DBN
             */
            std::string get_timed_name() const override ;

            // --------------------------------------------------------
            // Getters
            // --------------------------------------------------------
            const std::shared_ptr<NodeMetadata>& get_metadata() const {
                return metadata;
            }

            const std::unique_ptr<CPD>& get_cpd() const { return cpd; }

            int get_time_step() const { return time_step; }

            // --------------------------------------------------------
            // Setters
            // --------------------------------------------------------
            void set_assignment(Eigen::VectorXd assignment) {
                this->assignment = std::move(assignment);
            }

            void set_time_step(int time_step) {
                this->time_step = time_step;
            }
        };

    } // namespace model
} // namespace tomcat