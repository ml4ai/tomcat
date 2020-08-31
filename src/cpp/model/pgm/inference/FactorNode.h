#pragma once

#include "ExactInferenceNode.h"

#include "../../utils/Definitions.h"
#include "../cpd/CPD.h"

namespace tomcat {
    namespace model {

        //------------------------------------------------------------------
        // Forward declarations
        //------------------------------------------------------------------

        //------------------------------------------------------------------
        // Structs
        //------------------------------------------------------------------

        /**
         * Class description here
         */
        class FactorNode : public ExactInferenceNode {
          public:
            //------------------------------------------------------------------
            // Types, Enums & Constants
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            FactorNode(const std::string& label,
                       const Eigen::MatrixXd& potential_function,
                       const CPD::TableOrderingMap& ordering_map);

            ~FactorNode();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------

            // Copy constructor and assignment should be deleted to avoid
            // implicit slicing and loss of polymorphic behaviour in the
            // subclasses. To deep copy, the clone method must be used.
            FactorNode(const FactorNode&) = delete;

            FactorNode& operator=(const FactorNode&) = delete;

            FactorNode(FactorNode&&) = default;

            FactorNode& operator=(FactorNode&&) = default;

            //------------------------------------------------------------------
            // Operator overload
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Static functions
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Computes the product of all the incoming messages (except the one
             * coming from the destiny node) )and the potential function. Next,
             * marginalizes out the incoming nodes and returns the resultant
             * message.
             *
             * @return Outward message
             */
            Eigen::MatrixXd
            get_outward_message_to(const NodeName& node_name,
                                   int source_time_slice,
                                   int inference_time_step) const override;

            void replace_cpd_ordering_label(const std::string& current_label, const std::string& new_label);

            //------------------------------------------------------------------
            // Virtual functions
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Pure virtual functions
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Getters & Setters
            //------------------------------------------------------------------

          protected:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------

          private:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            // The potential function here is represented as a matrix that
            // corresponds to the conditional probability of a child node given
            // its parents. When calculating messages, sometimes we'll need to
            // pivot the table so that the child become one of the parents.
            // That's why this adjusted table is for. It will pivot the original
            // potential function according to the direction of the message.
            std::vector<Eigen::MatrixXd> get_incoming_messages_in_order(
                const NodeName& excluding_node_name,
                int source_time_slice,
                Eigen::MatrixXd& adjusted_potential_function) const;

            void fill_rotated_potential_functions();

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------

            Eigen::MatrixXd potential_function;

            // A potential function will actually be a CPD table that is
            // defined as the probability of a node (columns) given its parents
            // (rows). When computing messages in a factor graph, depending on
            // the direction of the message, we need to swap axis of the
            // potential function matrix, such that one of the parents assumes
            // the column axis and the main node takes that parent's place.
            // Since we are simulating a multi-dimensional array by using a
            // matrix, we need to perform some computation to do the swap.
            // This vector store all possible swaps where the index is the index
            // of the parent swapped.
            std::vector<Eigen::MatrixXd> rotated_potential_function;

            // The joint CPD is implemented as a single matrix where the rows
            // are combinations of parents' assignments for a node and the
            // columns are the node's assignments. We need to now the index of
            // each parent in this table to be able to index them correctly when
            // performing reductions or transformations in the matrix.
            CPD::TableOrderingMap ordering_map;
        };

    } // namespace model
} // namespace tomcat
