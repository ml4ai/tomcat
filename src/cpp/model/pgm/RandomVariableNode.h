#pragma once

#include "Node.h"

#include "../utils/Definitions.h"
#include "cpd/CPD.h"

namespace tomcat {
    namespace model {

        //------------------------------------------------------------------
        // Forward declarations
        //------------------------------------------------------------------

        class RandomVariableNode;

        //------------------------------------------------------------------
        // Structs
        //------------------------------------------------------------------

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

            /**
             * Marks the CPDs of the node as not updated.
             */
            void reset_cpd_updated_status();

            /**
             * Replaces parameter nodes in node dependent CPD templates by a
             * concrete timed-instance node in the unrolled DBN.
             *
             * @param parameter_nodes_map: mapping between a parameter node
             * timed name and its object in an unrolled DBN
             * @param time_step: time step of the node that owns the CPD in the
             * unrolled DBN. It can be different from the time step of the
             * parameter node if the latter is shared among nodes over several
             * time steps.
             */
            void update_cpd_templates_dependencies(NodeMap& parameter_nodes_map,
                                                   int time_step);

            /**
             * Create new references for the CPD templates of the node.
             */
            void clone_cpd_templates();

            /**
             * Generate samples from this node's CPD given its parents
             * assignments. If the node belongs to a plate, multiple samples are
             * generated: one for each in-plate copy. Otherwise a single sample
             * is returned.
             *
             * @param random_generator: random number generator
             * @param parent_nodes: list of parent nodes' timed instances
             * @param num_samples: number of samples to generate
             * @param equal_samples: whether the samples generated must be the
             * same
             *
             * @return Samples from the node's CPD.
             */
            Eigen::MatrixXd
            sample(std::shared_ptr<gsl_rng> random_generator,
                   const std::vector<std::shared_ptr<Node>>& parent_nodes,
                   int num_samples,
                   bool equal_samples = false) const;

            /**
             * Generate samples from this node's CPD scaled by some weights
             * given its parents assignments. If the node belongs to a plate,
             * multiple samples are generated: one for each in-plate copy.
             * Otherwise a single sample is returned.
             *
             * @param random_generator: random number generator
             * @param parent_nodes: list of parent nodes' timed instances
             * @param num_samples: number of samples to generate
             * @param weights: scale coefficients to the underlying distribution
             * @param equal_samples: whether the samples generated must be the
             * same
             *
             * @return Samples from the node's CPD.
             */
            Eigen::MatrixXd
            sample(std::shared_ptr<gsl_rng> random_generator,
                   const std::vector<std::shared_ptr<Node>>& parent_nodes,
                   int num_samples,
                   Eigen::MatrixXd weights,
                   bool equal_samples = false) const;

            /**
             * Samples a node using conjugacy properties and sufficient
             * statistics stored in the node's CPD.
             *
             * @param random_generator: random number generator
             * @return
             */
            Eigen::MatrixXd sample_from_conjugacy(
                std::shared_ptr<gsl_rng> random_generator,
                const std::vector<std::shared_ptr<Node>>& parent_nodes,
                int num_samples) const;

            /**
             * Get pdfs for the node's assignments given its parents'
             * assignments.
             *
             * @param parent_nodes: list of parent nodes' timed instances
             *
             * @return Pdfs relative to the node's assignments.
             */
            Eigen::VectorXd get_pdfs(
                const std::vector<std::shared_ptr<Node>>& parent_nodes) const;

            /**
             * Update sufficient statistics of parent parameter nodes with this
             * node's assignment(s).
             *
             * @param parent_nodes: list of parent nodes' timed instances
             */
            void update_parents_sufficient_statistics(
                const std::vector<std::shared_ptr<Node>>& parent_nodes);

            /**
             * Adds a value to the sufficient statistics of a parameter node's
             * CPD.
             *
             * @param sample: Sample to add to the sufficient statistics. The
             * update_parents_sufficient_statistics will call this function for
             * parameter nodes at some point.
             */
            void add_to_sufficient_statistics(const Eigen::VectorXd& sample);

            /**
             * Clear the values stored as sufficient statistics in the node's
             * CPD.
             */
            void reset_sufficient_statistics();

            /**
             * Prevents node's assignment to be changed.
             */
            void freeze();

            /**
             * Frees node's assignment to be changed.
             */
            void unfreeze();

            /**
             * Adds a CPD to the list of possible CPDs of the node.
             *
             * @param cpd: CPD
             */
            void add_cpd_template(std::shared_ptr<CPD>& cpd);

            /**
             * Adds CPD to the list of possible CPDs of the node.
             *
             * @param cpd: CPD
             */
            void add_cpd_template(std::shared_ptr<CPD>&& cpd);

            /**
             * Returns the node's CPD associated to a set of parents.
             *
             * @param parent_labels: labels of the parents of the node
             *
             * @return node's CPD related to the parents informed
             */
            std::shared_ptr<CPD>
            get_cpd_for(const std::vector<std::string>& parent_labels) const;

            // -----------------------------------------------------------------
            // Getters & Setters
            // -----------------------------------------------------------------
            int get_time_step() const;

            void set_time_step(int time_step);

            void set_assignment(Eigen::MatrixXd assignment);

            bool is_frozen() const;

            void set_cpd(const std::shared_ptr<CPD>& cpd);

            const std::shared_ptr<CPD>& get_cpd() const;

          private:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Create new reference for the CPD of the node.
             */
            void clone_cpd();

            /**
             * Copies data members of a random variable node.
             *
             * @param cpd: continuous CPD
             */
            void copy_node(const RandomVariableNode& node);

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

            // -----------------------------------------------------------------
            // Data members
            // -----------------------------------------------------------------

            // Time step where the node shows up in the unrolled DBN. This
            // variable will be assigned when a concrete timed instance of this
            // node is created and assigned to a vertex in an unrolled DBN.
            int time_step = 0;

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
            std::unordered_map<std::string, std::shared_ptr<CPD>> cpd_templates;

            // Conditional probability distribution of the timed instance of the
            // node in the unrolled DBN. The node template contains a list of
            // possible CPDs that can be associated with it. Once a concrete
            // timed instance of the node is created in the unrolled DBN, the
            // set of parents of such node is known and its CPD can be fully
            // determined.
            std::shared_ptr<CPD> cpd;

            /**
             * A frozen node will have it's assignment preserved and won't be
             * considered a latent node by the samplers. It will behave like a
             * constant node.
             */
            bool frozen = false;
        };

    } // namespace model
} // namespace tomcat