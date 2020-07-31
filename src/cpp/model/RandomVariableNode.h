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
          private:
            typedef std::unordered_map<std::string, std::shared_ptr<Node>>
                NodeMap;
            /**
             * Copy members of a random variable node.
             *
             * @param cpd: continuous CPD
             */
            void copy_from_node(const RandomVariableNode& node);

          protected:
            // CPD is a shared pointer because a multiple nodes can have a CPD
            // that depend on the same set of parameters.
            // A node can also have more than one CPD, each one referring to a
            // conditional distribution given a set of different parents. For
            // instance, a State node in a HMM will likely have two CPDs: one
            // for the first time step with no parent nodes associated and
            // another one to the following time steps with a State from the
            // previous time step as a parent.
            // The key in this data structure is a string that uniquely identify
            // the set of parents of the node to which the CPD is related.
            std::unordered_map<std::string, std::shared_ptr<CPD>> cpds;

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
            RandomVariableNode(std::shared_ptr<NodeMetadata>& metadata,
                               int time_step = 0)
                : Node(metadata), time_step(time_step) {}
            RandomVariableNode(std::shared_ptr<NodeMetadata>&& metadata,
                               int time_step = 0)
                : Node(std::move(metadata)), time_step(time_step) {}

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
             * Add CPD to the list of CPDs of the node
             *
             * @param cpd: CPD
             */
            void add_cpd(std::shared_ptr<CPD>& cpd);

            /**
             * Add CPD to the list of CPDs of the node
             *
             * @param cpd: CPD
             */
            void add_cpd(std::shared_ptr<CPD>&& cpd);

            /**
             * Return the node's CPD given a list of its parents.
             *
             * @param parent_labels: labels of the parents of the node
             * @return node's CPD related to the parents informed
             */
            std::shared_ptr<CPD>
            get_cpd_for(const std::vector<std::string>& parent_labels) const;

            /**
             * Replace a CPD by another pointer. The list of parents of the new
             * CPD has to be the same of the CPD to be replaced.
             *
             * @param cpd: new CPD
             */
            void replace_cpd(std::shared_ptr<CPD>& cpd);

            /**
             * Replace a CPD by another pointer. The list of parents of the new
             * CPD has to be the same of the CPD to be replaced.
             *
             * @param cpd: new CPD
             */
            //void replace_cpd(std::shared_ptr<CPD>&& cpd);

            /**
             * Mark CPDs of the node as not updated.
             */
            void reset_cpds_updated_status();

            /**
             * Replace parameter nodes in the node dependent CPDs by the correct
             * replica of the node in the unrolled DBN.
             *
             * @param parameter_nodes_map: map between a parameter node timed name and
             * it's node object in an unrolled DBN
             * @param time_step: time step of the node that owns the CPD in the
             * unrolled DBN
             */
            void update_cpd_dependencies(NodeMap& parameter_nodes_map,
                             int time_step);

            /**
             * Clone node's CPDs
             */
            void clone_cpds();

            /**
             * Set assignment from a single numeric value if samples from this
             * node is 1-dimensional.
             *
             * @param assignment: numeric value
             */
            void set_assignment(double assignment);

            void print(std::ostream& os) const override;

            std::string get_description() const override;

            std::unique_ptr<Node> clone() const override;

            std::string get_timed_name() const override;

            std::string get_timed_name(int time_step) const override;

            // --------------------------------------------------------
            // Getters
            // --------------------------------------------------------

            int get_time_step() const { return time_step; }

            // --------------------------------------------------------
            // Setters
            // --------------------------------------------------------
            void set_assignment(Eigen::VectorXd assignment) {
                this->assignment = std::move(assignment);
            }

            void set_time_step(int time_step) { this->time_step = time_step; }
        };

    } // namespace model
} // namespace tomcat