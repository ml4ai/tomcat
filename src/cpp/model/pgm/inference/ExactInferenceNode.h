#pragma once

#include <string>
#include <unordered_map>

#include <eigen3/Eigen/Dense>

#include "../../utils/Definitions.h"

namespace tomcat {
    namespace model {

        //------------------------------------------------------------------
        // Forward declarations
        //------------------------------------------------------------------

        /**
         * This class represents a node in a factor graph used for
         * exact inference with message passing algorithm.
         */
        class ExactInferenceNode {
          public:
            //------------------------------------------------------------------
            // Structs
            //------------------------------------------------------------------

            /**
             * Structure that contains a node label and the concrete time step
             * for inference of a node. Functions are implemented so that this
             * structure can be used as a key to an unordered map.
             */
            struct NodeName {
                struct Hash {
                    std::size_t operator()(const NodeName& node_name) const {
                        std::size_t h1 =
                            std::hash<std::string>()(node_name.label);
                        std::size_t h2 = std::hash<int>()(node_name.time_step);

                        return h1 ^ h2;
                    }
                };

                NodeName() {}
                NodeName(const std::string& label, int time_step)
                    : label(label), time_step(time_step) {}

                bool operator==(const NodeName& node_name) const {
                    return this->label == node_name.label &&
                           this->time_step == node_name.time_step;
                }

                static std::string get(const std::string& label,
                                       int time_step) {
                    std::stringstream ss;
                    ss << "(" << label << ", " << time_step << ")";
                    return ss.str();
                }

                std::string get() const {
                    return get(this->label, this->time_step);
                }

                std::string label;
                int time_step;
            };

            //------------------------------------------------------------------
            // Types, Enums & Constants
            //------------------------------------------------------------------

            enum Direction { backwards, forward };

            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates an abstract instance of an inference node.
             */
            ExactInferenceNode(const std::string& label);

            ~ExactInferenceNode();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------

            // Copy constructor and assignment should be deleted to avoid
            // implicit slicing and loss of polymorphic behaviour in the
            // subclasses. To deep copy, the clone method must be used.
            ExactInferenceNode(const ExactInferenceNode&) = delete;

            ExactInferenceNode& operator=(const ExactInferenceNode&) = delete;

            ExactInferenceNode(ExactInferenceNode&&) = default;

            ExactInferenceNode& operator=(ExactInferenceNode&&) = default;

            //------------------------------------------------------------------
            // Operator overload
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Static functions
            //------------------------------------------------------------------

            /**
             * We need unique labels because...
             * @param original_label
             * @param time_step
             * @return
             */
            static std::string
            get_unique_label_for(const std::string& original_label,
                                 int time_step);

            /**
             * Returns a unique name for a node given its label and a time step.
             *
             * @param node_label: node's label
             * @param time_step: time step where the node is
             *
             * @return Node's name
             */
            //            static std::string get_node_name(const std::string&
            //            node_label,
            //                                             int time_step);
            //
            //            /**
            //             * Returns the node's label from its name.
            //             *
            //             * @param node_name: node's name
            //             *
            //             * @return Node's label
            //             */
            //            static std::string
            //            get_node_label_from(const std::string& node_name);

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Returns a unique name of the node.
             *
             * @return Node's name
             */
            // std::string get_name() const;

            /**
             * Returns the incoming message from another node (source).
             *
             * @param node_label: source node's label
             * @param time_step: source node's time step
             *
             * @return Message
             */
            const Eigen::MatrixXd&
            get_incoming_message_from(const NodeName& node_name,
                                      int target_time_step) const;

            /**
             * Stores the incoming message from a given node (source)
             *
             * @param node_label: source node's label
             * @param time_step: source node's time step
             * @param message: message
             */
            void set_incoming_message_from(const NodeName& node_name,
                                           int target_time_step,
                                           const Eigen::MatrixXd& message);


            void set_data_at(int time_step);

            //------------------------------------------------------------------
            // Virtual functions
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Pure virtual functions
            //------------------------------------------------------------------

            /**
             * Computes the message that goes to the next node in a particular
             * traversal.
             *
             * @return Outward message
             */
            virtual Eigen::MatrixXd
            get_outward_message_to(const NodeName& node_name,
                                   int inference_time_slice,
                                   Direction direction) const = 0;

//            std::string get_original_label() const;

            //------------------------------------------------------------------
            // Getters & Setters
            //------------------------------------------------------------------
            const std::string& get_label() const;

          protected:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------
            std::string label;

            //            // A relative time step can be 0, 1 or 2. Beyond time
            //            step 2, the
            //            // structure of the dynamic bayes will be the same
            //            (parents and
            //            // child nodes don't change). Messages for nodes in
            //            above that time
            //            // step limit will be stored in a map inside the node
            //            in the last
            //            // possible time step given by the min(2, number of
            //            time steps the
            //            // DBN was unrolled).
            //            int relative_time_step;

            // This map stores messages tha arrive in this node. The key to the
            // first map if the time step of the node. The key to the second map
            // is given by the source node's name and the value is the message
            // that goes from the source node to this node.
            std::unordered_map<
                int,
                std::unordered_map<NodeName, Eigen::MatrixXd, NodeName::Hash>>
                incoming_messages_per_time_slice;

          private:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------
        };

    } // namespace model
} // namespace tomcat
