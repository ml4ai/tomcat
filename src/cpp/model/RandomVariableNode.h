#pragma once

#include "Node.h"

#include "CPD.h"

namespace tomcat {
    namespace model {

        /**
         * A random variable node is a concrete node in the unrolled DBN
         * that has a distribution from which it can be sampled from. The
         * assignment of this node can change as we sample from it's posterior
         * distribution over the other nodes' assignments in the unrolled DBN.
         */
        class RandomVariableNode : public Node {
          public:
            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates an instance of a random variable node. A random variable
             * node has CPDs associated with it and its assigned value is
             * mutable.
             *
             * @param metadata: node's metadata
             * @param time_step: node's time step in an unrolled DBN (0 by
             * default)
             */
            RandomVariableNode(std::shared_ptr<NodeMetadata>& metadata,
                               int time_step = 0);

            /**
             * Creates an instance of a random variable node. A random variable
             * node has CPDs associated with it and its assigned value is
             * mutable.
             *
             * @param metadata: node's metadata
             * @param time_step: node's time step in an unrolled DBN (0 by
             * default)
             */
            RandomVariableNode(std::shared_ptr<NodeMetadata>&& metadata,
                               int time_step = 0);

            ~RandomVariableNode();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------
            RandomVariableNode(const RandomVariableNode& node);

            RandomVariableNode& operator=(const RandomVariableNode& node);

            RandomVariableNode(RandomVariableNode&&) = default;

            RandomVariableNode& operator=(RandomVariableNode&&) = default;

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------
            std::string get_description() const override;

            std::unique_ptr<Node> clone() const override;

            std::string get_timed_name() const override;

            //std::string get_timed_name(int time_step) const override;

            /**
             * Adds a CPD to the list of CPDs of the node.
             *
             * @param cpd: CPD
             */
            void add_cpd(std::shared_ptr<CPD>& cpd);

            /**
             * Adds CPD to the list of CPDs of the node.
             *
             * @param cpd: CPD
             */
            void add_cpd(std::shared_ptr<CPD>&& cpd);

            /**
             * Returns the node's CPD associated to a set of parents.
             *
             * @param parent_labels: labels of the parents of the node
             *
             * @return node's CPD related to the parents informed
             */
            std::shared_ptr<CPD>
            get_cpd_for(const std::vector<std::string>& parent_labels) const;

            /**
             * Marks the CPDs of the node as not updated.
             */
            void reset_cpds_updated_status();

            /**
             * Replaces parameter nodes in node dependent CPDs by a concrete
             * timed-instance node in the unrolled DBN.
             *
             * @param parameter_nodes_map: mapping between a parameter node
             * timed name and its object in an unrolled DBN
             * @param time_step: time step of the node that owns the CPD in the
             * unrolled DBN. It can be different from the time step of the
             * parameter node if the latter is shared among nodes over several
             * time steps.
             */
            void update_cpd_dependencies(NodeMap& parameter_nodes_map,
                                         int time_step);

            /**
             * Create new references for the CPDs of the node.
             */
            void clone_cpds();

            /**
             * Assigns a 1D-vector comprised by the informed numerical value as
             * assignment of the node's sample size is one.
             *
             * @param assignment: numeric value
             */
            void set_assignment(double assignment);

            // -----------------------------------------------------------------
            // Getters & Setters
            // -----------------------------------------------------------------
            int get_time_step() const;

            void set_time_step(int time_step);

            void set_assignment(Eigen::VectorXd assignment);

          protected:
            // -----------------------------------------------------------------
            // Data members
            // -----------------------------------------------------------------

            // CPD is a shared pointer because multiple nodes can have a CPD
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

            // Time step where the node shows up in the unrolled DBN. This
            // variable will be assigned when a concrete timed instance of this
            // node is created and assigned to a vertex in an unrolled DBN.
            int time_step;

          private:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Copies data members of a random variable node.
             *
             * @param cpd: continuous CPD
             */
            void copy_from_node(const RandomVariableNode& node);

            /**
             * Sorts a list of labels and concatenate them into a string
             * separated by a delimiter.
             *
             * @param labels: list of random variable node's labels *
             * @return Unique string formed by a list of random variable node's
             * labels
             */
            std::string
            get_unique_key_from_labels(std::vector<std::string> labels) const;
        };

    } // namespace model
} // namespace tomcat