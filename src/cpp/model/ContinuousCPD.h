#pragma once

#include "CPD.h"
#include "Node.h"
#include <iostream>
#include <memory>

namespace tomcat {
    namespace model {

        /**
         * A Continuous CPD consists of a table containing the
         * parameters of the continuous distribution of the node that is sampled
         * from this CPD given its parents's assignments. The number of rows is
         * given by the product of the cardinalities of the parent nodes of such
         * node. Each row represents a combination of possible assignments of
         * the parent nodes ordered with respect to the binary basis.
         *
         * For instance,
         *
         * Let A and B be parents of the node C.
         *
         * A -> C B -> C
         *
         * Suppose A, B have cardinalities 2, 3 respectively and C is sampled
         * from an arbitrary continuous distribution with parameter vector
         * \f$\phi\f$ of size 3.
         *
         * A ContinuousCPD for C will be as follows,
         * |------------------------------------|
         * | A | B |////////////////////////////|
         * |------------------------------------|
         * | 0 | 0 |      \f$\phi_{00}\f$       |
         * |------------------------------------|
         * | 0 | 1 |      \f$\phi_{01}\f$       |
         * |------------------------------------|
         * | 0 | 2 |      \f$\phi_{02}\f$       |
         * |------------------------------------|
         * | 1 | 0 |      \f$\phi_{10}\f$       |
         * |------------------------------------|
         * | 1 | 1 |      \f$\phi_{11}\f$       |
         * |------------------------------------|
         * | 1 | 2 |      \f$\phi_{12}\f$       |
         * |------------------------------------|
         */
        class ContinuousCPD : public CPD {
          protected:
            std::vector<std::vector<std::shared_ptr<Node>>> parameter_table;

          public:
            ContinuousCPD() {}
            ContinuousCPD(std::vector<std::string> parent_node_label_order)
                : CPD(std::move(parent_node_label_order)) {}

            ~ContinuousCPD() {}

            // Copy constructor and assignment should be deleted to avoid
            // implicit slicing and loss of polymorphic behaviour in the
            // subclasses. To deep copy, the clone method must be used.
            ContinuousCPD(const ContinuousCPD&) = delete;
            ContinuousCPD& operator=(const ContinuousCPD&) = delete;

            ContinuousCPD(ContinuousCPD&& cpd) = default;
            ContinuousCPD& operator=(ContinuousCPD&& cpd) = default;

            /**
             * Copy members of a continuous CPD.
             *
             * @param cpd: continuous CPD
             */
            void copy_from_cpd(const ContinuousCPD& cpd);

            /**
             * Sample a vector for each combination of parent nodes' assignments
             * (each row of the cpd table).
             *
             * @param generator: random number generator
             * @return matrix of sampled values. Each row contains a vector
             *  \f$\theta\f$ sampled from a continuous distribution with
             *  parameter vector \f$\phi\f$ defined in each row of the
             *  parameter table.
             */
            virtual Eigen::MatrixXd
            sample(std::shared_ptr<gsl_rng> generator) const override = 0;

            virtual void print(std::ostream& os) const override = 0;

            virtual std::unique_ptr<CPD> clone() const override = 0;

            /**
             * Transform a table of numeric values for \f$\phi\f$ to a list of
             * constant vector nodes to keep static and node dependent CPDs
             * compatible.
             *
             * @param parent_node_label_order: evaluation order of the parent
             * nodes assignment for correct table indexing
             * @param parameter_table: matrix containing constant numerical
             * values for \f$\phi\f$
             */
            virtual void init_from_matrix(Eigen::MatrixXd& parameter_table) = 0;

            virtual void
            update_dependencies(NodeMap& parameter_nodes_map, int time_step) override;
        };

    } // namespace model
} // namespace tomcat
