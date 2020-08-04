#pragma once

#include "ContinuousCPD.h"

namespace tomcat {
    namespace model {

        /**
         * A Dirichlet CPD consists of a table containing \f$\alpha\f$: the
         * parameters of a Dirichlet (or Beta if the size of the vector
         * \f$\alpha\f$ is 2) distribution of the node that is sampled from this
         * CPD given its parents' assignments. The number of rows is given by
         * the product of the cardinalities of these parent nodes. Each row
         * represents a combination of possible assignments of the parent nodes
         * ordered with respect to the binary basis.
         *
         * For instance,
         *
         * Let A and B be parents of the node C.
         *
         * A -> C B -> C
         *
         * Suppose A, B have cardinalities 2, 3 respectively and C is sampled
         * from a dirichlet distribution with parameter vector \f$\alpha\f$ of
         * size 3.
         *
         * A DirichletCPD for C will be as follows,
         * ______________________________________
         * |///| C |       \f$\alpha\f$         |
         * |------------------------------------|
         * | A | B |////////////////////////////|
         * |------------------------------------|
         * | 0 | 0 |     \f$\alpha_{00}\f$      |
         * |------------------------------------|
         * | 0 | 1 |     \f$\alpha_{01}\f$      |
         * |------------------------------------|
         * | 0 | 2 |     \f$\alpha_{02}\f$      |
         * |------------------------------------|
         * | 1 | 0 |     \f$\alpha_{10}\f$      |
         * |------------------------------------|
         * | 1 | 1 |     \f$\alpha_{11}\f$      |
         * |------------------------------------|
         * | 1 | 2 |     \f$\alpha_{12}\f$      |
         * |------------------------------------|
         */

        class DirichletCPD : public ContinuousCPD {
          public:
            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates an instance of a Dirichlet CPD comprised by a list of
             * node dependent parameters.
             *
             * @param parent_node_label_order: evaluation order of the parent
             * nodes' assignments for correct table indexing
             * @param parameter_table: list of parameter vector \f$\alpha\f$
             * determined by other nodes' assignments
             */
            DirichletCPD(std::vector<std::string>& parent_node_label_order,
                         std::vector<std::shared_ptr<Node>>& parameter_table);

            /**
             * Creates an instance of a Dirichlet CPD comprised by a list of
             * node dependent parameters.
             *
             * @param parent_node_label_order: evaluation order of the parent
             * nodes' assignments for correct table indexing
             * @param parameter_table: list of parameter vector \f$\alpha\f$
             * determined by other nodes' assignments
             */
            DirichletCPD(std::vector<std::string>&& parent_node_label_order,
                         std::vector<std::shared_ptr<Node>>&& parameter_table);

            /**
             * Creates an instance of a Dirichlet CPD table by transforming a
             * table of parameter values to a list of constant vector nodes to
             * keep static and node dependent CPDs compatible.
             *
             * @param parent_node_label_order: evaluation order of the parent
             * nodes' assignments for correct table indexing
             * @param cpd_table: matrix containing constant numerical values for
             * \f$\alpha\f$
             */
            DirichletCPD(std::vector<std::string>& parent_node_label_order,
                         const Eigen::MatrixXd& parameter_values);

            /**
             * Creates an instance of a Dirichlet CPD table by transforming a
             * table of parameter values to a list of constant vector nodes to
             * keep static and node dependent CPDs compatible.
             *
             * @param parent_node_label_order: evaluation order of the parent
             * nodes' assignments for correct table indexing
             * @param cpd_table: matrix containing constant numerical values for
             * \f$\alpha\f$
             */
            DirichletCPD(std::vector<std::string>&& parent_node_label_order,
                         const Eigen::MatrixXd&& parameter_values);

            ~DirichletCPD();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------
            DirichletCPD(const DirichletCPD& cpd);

            DirichletCPD& operator=(const DirichletCPD& cpd);

            DirichletCPD(DirichletCPD&& cpd) = default;

            DirichletCPD& operator=(DirichletCPD&& cpd) = default;

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------
            std::unique_ptr<CPD> clone() const override;

            std::string get_description() const override;

          protected:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Samples a value from a Dirichlet distribution comprised by the
             * parameter vector \f$\alpha\f$ assigned to one of the nodes in the
             * parameter table.
             *
             * @param random_generator: random number generator
             * @param table_row: row of the parameter table containing the
             * node that stores the \f$\alpha\f$
             * @return Value sampled from a Dirichlet distribution
             */
            virtual Eigen::VectorXd
            sample_from_table_row(std::shared_ptr<gsl_rng> random_generator,
                                  int table_row) const override;

            /**
             * Transform a table of numeric values for \f$\alpha\f$ to a list of
             * constant vector nodes to keep static and node dependent CPDs
             * compatible.
             *
             * @param matrix: matrix containing constant numerical
             * values for \f$\alpha\f$
             */
            void init_from_matrix(const Eigen::MatrixXd& matrix) override;

          private:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Stores the parameter table in the CPD. Instead of storing each
             * term of the parameter vector \f$\alpha\f$ in a different node,
             * the whole \f$\alpha\f$ will be stored as an assignment of such
             * node. This function converts a list of nodes to a list of list of
             * a single node to comply with the format of the parameter table
             * data member.
             *
             * @param parameter_table: vector of nodes containing \f$\alpha\f$
             */
            void init_from_table(
                std::vector<std::shared_ptr<Node>>& parameter_table);
        };

    } // namespace model
} // namespace tomcat
