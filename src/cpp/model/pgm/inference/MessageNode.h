#pragma once

#include <memory>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

#include <eigen3/Eigen/Dense>

#include "../../utils/Definitions.h"

namespace tomcat {
    namespace model {

        /**
         * This class represents a node in a factor graph used for
         * exact inference with message passing algorithms.
         */
        class MessageNode {
          public:
            //------------------------------------------------------------------
            // Structs
            //------------------------------------------------------------------

            /**
             * This struct stores a map between a node's name (label + time
             * step) and the message the emitted by this node. It's used to
             * store the incoming messages of a node where the key node names in
             * this struct would indicate the nodes the messages came from.
             */
            struct MessageContainer {
                std::unordered_map<std::string, Eigen::MatrixXd>
                    node_name_to_messages;

                const Eigen::MatrixXd
                get_message_for(const std::string node_label,
                                int time_step) const {

                    std::string node_name =
                        MessageNode::get_name(node_label, time_step);
                    return this->node_name_to_messages.at(node_name);
                }

                void set_message_for(const std::string node_label,
                                     int time_step,
                                     const Eigen::MatrixXd& message) {

                    std::string node_name =
                        MessageNode::get_name(node_label, time_step);
                    this->node_name_to_messages[node_name] = message;
                }

                int size() const { return this->node_name_to_messages.size(); }
            };

            //------------------------------------------------------------------
            // Types, Enums & Constants
            //------------------------------------------------------------------
            inline static const std::string PRIOR_NODE_LABEL = "";
            inline static const std::string END_NODE_LABEL = "end_factor";

            // Direction of the message being passed in the factor graph
            enum Direction { backwards, forward };

            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates an empty abstract instance of a message node.
             */
            MessageNode();

            /**
             * Creates an abstract instance of a message node.
             */
            MessageNode(const std::string& label, int time_step);

            virtual ~MessageNode();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------

            // Copy constructor and assignment should be deleted to avoid
            // implicit slicing and loss of polymorphic behaviour in the
            // subclasses. To deep copy, the clone method must be used.
            MessageNode(const MessageNode&) = delete;

            MessageNode& operator=(const MessageNode&) = delete;

            MessageNode(MessageNode&&) = default;

            MessageNode& operator=(MessageNode&&) = default;

            //------------------------------------------------------------------
            // Static functions
            //------------------------------------------------------------------

            /**
             * Returns a unique name for a label and time step.
             *
             * @param label: node's label
             * @param time_step: node's time step
             *
             * @return Unique node name
             */
            static std::string get_name(const std::string& label,
                                        int time_step);

            /**
             * Indicates whether a node is a prior from its name. A prior is
             * defined as an abstraction to represent the node tha would be
             * linked to a factor node that has no parents.
             *
             * @param node_name: node's name
             *
             * @return True if it's a prior node.
             */
            static bool is_prior(const std::string& node_name);

            /**
             * Indicates whether a node is an end node from its name. An end
             * node is defined as an abstraction to represent the node tha would
             * be linked to a variable node that has no children.
             *
             * @param node_name: node's name
             *
             * @return True if it's an end node.
             */
            static bool is_end_node(const std::string& node_name);

            /**
             * Splits the a node's name to retrieve its label and time step as
             * separate variables.
             *
             * @param node_name: node's name
             *
             * @return Node's label and time step.
             */
            static std::pair<std::string, int>
            strip(const std::string& node_name);

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * For a given target time step, returns the message that came from
             * a source node.
             *
             * @param source_label: source node's label
             * @param source_time_step: source node's time step
             * @param target_time_step: time step instance of this node to check
             * the incoming message
             *
             * @return Message
             */
            Eigen::MatrixXd
            get_incoming_message_from(const std::string& source_label,
                                      int source_time_step,
                                      int target_time_step) const;

            /**
             * For a given target time step, stores the message that came from
             * a source node.
             *
             * @param source_label: source node's label
             * @param source_time_step: source node's time step
             * @param target_time_step: time step instance of this node to store
             * the incoming message
             */
            void set_incoming_message_from(const std::string& source_label,
                                           int source_time_step,
                                           int target_time_step,
                                           const Eigen::MatrixXd& message);

            /**
             * Get template node's name as a combination of its label and time
             * step.
             *
             * @return Node's name
             */
            std::string get_name() const;

            //------------------------------------------------------------------
            // Virtual functions
            //------------------------------------------------------------------

            /**
             * Clears messages and beyond a given time step (not inclusive).
             *
             * @param time_step: time step
             */
            virtual void erase_incoming_messages_beyond(int time_step);

            //------------------------------------------------------------------
            // Pure virtual functions
            //------------------------------------------------------------------

            /**
             * Computes the message from this node to another in a given time
             * step.
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
            virtual Eigen::MatrixXd get_outward_message_to(
                const std::shared_ptr<MessageNode>& template_target_node,
                int template_time_step,
                int target_time_step,
                Direction direction) const = 0;

            /**
             * Whether the node is a factor node. The node instance type could
             * be used to check this information but for better readability,
             * this function was defined;
             *
             * @return True if the node in an instance of a factor node.
             */
            virtual bool is_factor() const = 0;

            //------------------------------------------------------------------
            // Getters & Setters
            //------------------------------------------------------------------
            const std::string& get_label() const;

            int get_time_step() const;

          protected:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Copies data members from another message node.
             *
             * @param node: other message node
             */
            void copy_node(const MessageNode& node);

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------
            std::string label;

            int time_step;

            // Max time step with stored messages
            int max_time_step_stored = 0;

            // This map stores messages tha arrive in this node. The key to the
            // map is the time step of the messages. This is needed because a
            // node template in the repeatable time step of the factor graph
            // stores incoming messages for all of its time-sliced "copies" in
            // the future.
            std::unordered_map<int, MessageContainer>
                incoming_messages_per_time_slice;
        };

    } // namespace model
} // namespace tomcat
