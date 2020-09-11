#pragma once

#include "MessageNode.h"

#include "model/utils/Definitions.h"

namespace tomcat {
    namespace model {

        /**
         * This class represents a variable node in a factor graph.
         */
        class VariableNode : public MessageNode {
          public:
            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates an instance of a variable node.
             *
             * @param label: node's label
             * @param time_step: node's time step
             * @param cardinality: node's cardinality
             */
            VariableNode(const std::string& label,
                         int time_step,
                         int cardinality);

            ~VariableNode();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------
            VariableNode(const VariableNode& node);

            VariableNode& operator=(const VariableNode& node);

            VariableNode(VariableNode&&) = default;

            VariableNode& operator=(VariableNode&&) = default;

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Computes the element-wise product of all the incoming messages
             * excluding the one that comes from the target node.
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

            /**
             * Computes the marginal distribution for a given node in a certain
             * point in time. The marginal is given by the multiplication of all
             * the incoming messages to the node.
             *
             * @param time_step: time step for inference
             *
             * @return Marginal distribution
             */
            Eigen::MatrixXd get_marginal_at(int time_step) const;

            /**
             * Stores data for the node in a given time step. If there's data in
             * a given time step, messages from this node in that time step are
             * deterministic.
             *
             * @param time_step: time step where the data must be set
             * @param data: vector with node's values from several data sets
             *
             */
            void set_data_at(int time_step, const Eigen::VectorXd& data);

            /**
             * If there's data assigned to a node in a given time step, remove
             * it from there.
             *
             * @param time_step: time step to remove the data from
             *
             */
            void erase_data_at(int time_step);

            //------------------------------------------------------------------
            // Getters & Setters
            //------------------------------------------------------------------
            int get_cardinality() const;

          private:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Copies data members from another variable node.
             *
             * @param node: other variable node
             */
            void copy_node(const VariableNode& node);

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------
            int cardinality;

            // Stores observable data for the node per time slice. The rows of
            // the data matrix contains a one-hot vector encode representing the
            // value observed for a particular data point. The number of rows in
            // the matrix is the number of data points observed.
            std::unordered_map<int, Eigen::MatrixXd> data_per_time_slice;
        };

    } // namespace model
} // namespace tomcat
