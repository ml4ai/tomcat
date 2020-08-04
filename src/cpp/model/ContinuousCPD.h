#pragma once

#include "CPD.h"

namespace tomcat {
    namespace model {

        /**
         * A Continuous CPD consists of a table containing the
         * parameters of the continuous distribution of the node that is sampled
         * from this CPD given its parents's assignments. The number of rows is
         * given by the product of the cardinalities of those parent nodes. Each
         * row represents a combination of their possible assignments ordered
         * with respect to the binary basis.
         *
         * For instance,
         *
         * Let A and B be parents of the node C.
         *
         * A -> C B -> C
         *
         * Suppose A, B have cardinalities 2, 3 respectively and C is sampled
         * from an arbitrary continuous distribution with parameter vector
         * \f$\phi\f$.
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
          public:
            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates an abstract representation of a Continuous CPD.
             */
            ContinuousCPD();

            /**
             * Creates an abstract representation of a Continuous CPD. This
             * constructor is marked as explicit because it takes only one
             * parameter what makes it easier to be implicitly instantiated.
             * Explicit instantiation is preferred to avoid hard to catch
             * errors.
             *
             * @param parent_node_label_order: evaluation order of the parent
             * nodes assignment for correct table indexing
             */
            explicit ContinuousCPD(
                std::vector<std::string>& parent_node_label_order);

            /**
             * Creates an abstract representation of a Continuous CPD. This
             * constructor is marked as explicit because it takes only one
             * parameter what makes it easier to be implicitly instantiated.
             * Explicit instantiation is preferred to avoid hard to catch
             * errors.
             *
             * @param parent_node_label_order: evaluation order of the parent
             * nodes assignment for correct table indexing
             */
            explicit ContinuousCPD(
                std::vector<std::string>&& parent_node_label_order);

            virtual ~ContinuousCPD();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------

            // Copy constructor and assignment should be deleted to avoid
            // implicit slicing and loss of polymorphic behaviour in the
            // subclasses. For deep copy, the clone method must be used.
            ContinuousCPD(const ContinuousCPD&) = delete;

            ContinuousCPD& operator=(const ContinuousCPD&) = delete;

            ContinuousCPD(ContinuousCPD&& cpd) = default;

            ContinuousCPD& operator=(ContinuousCPD&& cpd) = default;

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------
            virtual void update_dependencies(Node::NodeMap& parameter_nodes_map,
                                             int time_step) override;

          protected:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Copy data members of a continuous CPD.
             *
             * @param cpd: continuous CPD
             */
            void copy_from_cpd(const ContinuousCPD& cpd);

            void clone_nodes() override;

            //------------------------------------------------------------------
            // Pure virtual functions
            //------------------------------------------------------------------

            /**
             * Fills the probability table with constant nodes created from a
             * matrix of numeric values for \f$\phi\f$.
             *
             * @param matrix: matrix of values for \f$\phi\f$
             */
            virtual void init_from_matrix(const Eigen::MatrixXd& matrix) = 0;

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------
            std::vector<std::vector<std::shared_ptr<Node>>> parameter_table;
        };

    } // namespace model
} // namespace tomcat
