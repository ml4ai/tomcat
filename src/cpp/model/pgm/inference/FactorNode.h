#pragma once

#include "MessageNode.h"

#include "../../utils/Definitions.h"
#include "../cpd/CPD.h"

namespace tomcat {
    namespace model {

        /**
         * This class represents a factor node in a factor graph.
         */
        class FactorNode : public MessageNode {
          public:
            //------------------------------------------------------------------
            // Structs
            //------------------------------------------------------------------
            struct PotentialFunction {
                // The joint CPD table is implemented as a single matrix where
                // the rows are combinations of parents' assignments for a node
                // and the columns are the node's assignments. We need to now
                // the index of each parent in this table to be able to index
                // them correctly when performing reductions or transformations
                // in the matrix. This information is stored in the ordering
                // map.
                CPD::TableOrderingMap ordering_map;

                // CPD table
                Eigen::MatrixXd matrix;

                // Node's label in P(Node | ...)
                std::string main_node_label;

                // The node's label can be the same as one of its parents. For
                // instance, if the CPD table defines a transition matrix. The
                // labels are the same but the time step. The matrix will need
                // to be rotated to deal with backward messages, so this
                // attribute keeps track of duplicate keys so the matrix can be
                // correctly indexed even when rotated. In this context, a
                // rotation means replacing the node position in the matrix  by
                // one of it's parents. Suppose a CPD defines the probability
                // P(S|S,Q), the rotation P(Q|S,S) will cause a problem if
                // duplicate keys are not correctly handled.
                std::string duplicate_key = "";

                PotentialFunction() {}

                PotentialFunction(const CPD::TableOrderingMap& ordering_map,
                                  const Eigen::MatrixXd& matrix,
                                  const std::string main_node_label)
                    : ordering_map(ordering_map), matrix(matrix),
                      main_node_label(main_node_label) {}

                static std::string
                get_alternative_key_label(const std::string& label) {
                    return label + "*";
                }
            };

            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates an instance of a factor node.
             *
             * @param label: node's label. The factor's label will be a
             * modified version of the label informed to indicate that it's a
             * factor.
             * @param time_step: factor's time step
             * @param potential_function: matrix representing the potential
             * function
             * @param ordering_map: potential function matrix's ordering map
             */
            FactorNode(const std::string& label,
                       int time_step,
                       const Eigen::MatrixXd& potential_function,
                       const CPD::TableOrderingMap& ordering_map,
                       const std::string cpd_node_label);

            ~FactorNode();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------
            FactorNode(const FactorNode& node);

            FactorNode& operator=(const FactorNode& node);

            FactorNode(FactorNode&&) = default;

            FactorNode& operator=(FactorNode&&) = default;

            //------------------------------------------------------------------
            // Static functions
            //------------------------------------------------------------------

            /**
             * Adds a prefix to a label to indicate that it's a factor node's
             * label.
             *
             * @param original_label: label used in the composition.
             *
             * @return Factor node label
             */
            static std::string compose_label(const std::string& original_label);

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Computes the product of all the incoming messages (except the one
             * coming from the target node) and the potential function. Next,
             * marginalizes out the incoming nodes and returns the resultant
             * message.
             *
             * @param template_target_node: template instance of the node where
             * the message should go to
             * @param template_time_step: time step of this node where to get
             * the incoming messages from. If the template node belongs to the
             * repeatable structure, this information is needed to know which
             * time step to address to retrieve the incoming messages.
             * @param target_time_step: real time step of the target node
             * @param direction: direction of the message passing
             *
             * @return Message
             */
            Eigen::MatrixXd get_outward_message_to(
                const std::shared_ptr<MessageNode>& template_target_node,
                int template_time_step,
                int target_time_step,
                Direction direction) const override;

            bool is_factor() const override;

          private:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Computes and stores all possible rotations of the original
             * potential function.
             */
            void adjust_potential_functions();

            /**
             * Copies data members from another factor node.
             *
             * @param node: other factor node
             */
            void copy_node(const FactorNode& node);

            /**
             * Returns the messages that arrive into the factor node (ignoring a
             * given target node) in the order defined by its parents nodes in
             * the potential function ordering map. This is crucial to correclty
             * multiply incoming messages with the potential function matrix in
             * a correct way.
             *
             * @param ignore_label: label of the node that must be ignored. If
             * there's any, messages from this node in the target time step must
             * be discarded as this node is the final destination of the
             * messages we aim to compute when calling this function.
             * @param template_time_step: time step of this node where to get
             * the incoming messages from. If the template node belongs to the
             * repeatable structure, this information is needed to know which
             * time step to address to retrieve the incoming messages.
             * @param target_time_step: real time step of the target node
             * @param potential_function: potential function
             *
             * @return Incoming messages
             */
            std::vector<Eigen::MatrixXd> get_incoming_messages_in_order(
                const std::string& ignore_label,
                int template_time_step,
                int target_time_step,
                const PotentialFunction& potential_function) const;

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------
            PotentialFunction original_potential_function;

            // The potential function here is represented as a matrix that
            // corresponds to the conditional probability of a child node given
            // its parents. When calculating messages, depending on the
            // direction the message flows, the potential function matrix needs
            // to be rotated so that the child become one of the parents. That's
            // what this adjusted table is for. It will store all possible
            // rotations of the potential function matrix according to the the
            // node that should assume the child's position.
            std::unordered_map<std::string, PotentialFunction>
                node_label_to_rotated_potential_function;
        };

    } // namespace model
} // namespace tomcat
