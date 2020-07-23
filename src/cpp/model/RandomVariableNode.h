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
            // CPD is a shared pointer because a multiple nodes can have a CPD
            // that depend on the same set of parameters.
            std::shared_ptr<CPD> cpd;

            // Time step where the node is drawn in the unrolled DBN. This
            // variable will be assigned when this node is created as a vertex
            // in an unrolled DBN.
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
                               std::shared_ptr<CPD> cpd,
                               int time_step = 0)
                : Node(std::move(metadata)), cpd(std::move(cpd)),
                  time_step(time_step) {}

            ~RandomVariableNode() {}

            RandomVariableNode(const RandomVariableNode& node) {
                this->copy_from_node(node);
            }
            RandomVariableNode& operator=(const RandomVariableNode& node) {
                this->copy_from_node(node);
                return *this;
            }
            RandomVariableNode(RandomVariableNode&&) = default;
            RandomVariableNode& operator=(RandomVariableNode&&) = default;

            /**
             * Copy members of a random variable node.
             *
             * @param cpd: continuous CPD
             */
            void copy_from_node(const RandomVariableNode& node);

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
            std::string get_timed_name() const override;

            /**
             * Return the node name formed by it's label and an arbitrary
             * time_step
             *
             * @return node name in the unrolled DBN
             */
            std::string get_timed_name(int time_step) const override;

            // --------------------------------------------------------
            // Getters
            // --------------------------------------------------------
            const std::shared_ptr<CPD>& get_cpd() const { return cpd; }

            int get_time_step() const { return time_step; }

            // --------------------------------------------------------
            // Setters
            // --------------------------------------------------------
            void set_assignment(Eigen::VectorXd assignment) {
                this->assignment = std::move(assignment);
            }

            void set_time_step(int time_step) { this->time_step = time_step; }

            void set_cpd(std::shared_ptr<CPD> cpd) {
                RandomVariableNode::cpd = std::move(cpd);
            }
        };

    } // namespace model
} // namespace tomcat