#pragma once

#include <unordered_map>

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
            // Structs
            //------------------------------------------------------------------
            struct PotentialFunction {
                // The joint CPD is implemented as a single matrix where the
                // rows are combinations of parents' assignments for a node and
                // the columns are the node's assignments. We need to now the
                // index of each parent in this table to be able to index them
                // correctly when performing reductions or transformations in
                // the matrix.
                CPD::TableOrderingMap ordering_map;

                Eigen::MatrixXd matrix;

                std::string duplicate_key = "";

                PotentialFunction() {}

                PotentialFunction(const CPD::TableOrderingMap& ordering_map,
                                  const Eigen::MatrixXd& matrix)
                    : ordering_map(ordering_map), matrix(matrix) {}
            };

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
            static std::string get_factor_label_for_node(
                const std::string& non_factor_label);

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
                                   int inference_time_slice,
                                   Direction direction) const override;

//            void replace_cpd_ordering_label(const std::string& current_label,
//                                            const std::string& new_label);

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
                const NodeName& ignore_node_name,
                int inference_time_step,
                const PotentialFunction& potential_function) const;

            void adjust_potential_functions();

            std::string get_child_non_factor_label() const;

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------

            PotentialFunction original_potential_function;

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
            std::unordered_map<std::string, PotentialFunction>
                node_label_to_rotated_potential_function;
        };

    } // namespace model
} // namespace tomcat
