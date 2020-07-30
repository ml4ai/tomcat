#pragma once

#include "Node.h"
#include <eigen3/Eigen/Dense>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_rng.h>
#include <unordered_map>
#include <vector>

namespace tomcat {
    namespace model {

        /**
         * Abstract representation of a conditional probability distribution.
         *
         * In a CPD table, each row consists of the distribution of a child node
         * given its parents assignments. For the discrete case, each row
         * consists of probabilities that define a categorical distribution. For
         * the continuous case, each row contains the parameters of the
         * underlying distribution.
         *
         */
        class CPD {
          protected:
            typedef std::unordered_map<std::string, std::shared_ptr<Node>>
                NodeMap;

            // It defines the order of the parent nodes in the cartesian
            // product of their possible assignments. It's necessary to know
            // this order for correctly index a distribution given a parent
            // assignment.
            std::vector<std::string> parent_node_label_order;

            // Indicates whether the CPD is updated with the correct instances
            // of the nodes it depends on
            bool updated;

          public:
            CPD() {}
            /**
             * Create an abstract representation of a Conditional Probability
             * Distribution
             *
             * @param parent_node_label_order: evaluation order of the parent
             * nodes assignment for correct table indexing
             */
            CPD(std::vector<std::string> parent_node_label_order)
                : parent_node_label_order(std::move(parent_node_label_order)) {}

            virtual ~CPD() {}

            // Copy constructor and assignment should be deleted to avoid
            // implicit slicing and loss of polymorphic behaviour in the
            // subclasses. To deep copy, the clone method must be used.
            CPD(const CPD&) = delete;
            CPD& operator=(const CPD&) = delete;

            CPD(CPD&& cpd) = default;
            CPD& operator=(CPD&& cpd) = default;

            friend std::ostream& operator<<(std::ostream& os, const CPD& cpd) {
                cpd.print(os);
                return os;
            };

            /**
             * Print a short description of the distribution.
             *
             * @param os: output stream
             */
            virtual void print(std::ostream& os) const {}

            /**
             * Draw a sample from each distribution of the CPD table.
             *
             * @param random_generator: random number random_generator
             *
             * @return matrix or vector with the samples
             */
            virtual Eigen::MatrixXd
            sample(std::shared_ptr<gsl_rng> random_generator) const = 0;

            /**
             * Draw a sample from each distribution of the CPD table.
             *
             * @param random_generator: random number random_generator
             * @param index: row of the CPD table containing the distribution to
             * be sampled from
             *
             * @return matrix or vector with the samples
             */
            virtual Eigen::VectorXd
            sample(std::shared_ptr<gsl_rng> random_generator,
                   int index) const = 0;

            /**
             * Clone CPD
             *
             * @return pointer to the new CPD
             */
            virtual std::unique_ptr<CPD> clone() const = 0;

            /**
             * Replace parameter nodes in a node dependent CPD by the correct
             * replica of the node in the unrolled DBN.
             *
             * @param name_to_node: map between a parameter node timed name and
             * it's node object in an unrolled DBN
             * @param time_step: time step of the node that owns the CPD in the
             * unrolled DBN
             */
            virtual void update_dependencies(NodeMap& parameter_nodes_map,
                                             int time_step) = 0;

            void reset_updated() { this->updated = false; }

            bool is_updated() const { return this->updated; }

            // --------------------------------------------------------
            // Getters
            // --------------------------------------------------------
            const std::vector<std::string>&
            get_parent_node_label_order() const {
                return parent_node_label_order;
            }
        };

    } // namespace model
} // namespace tomcat
