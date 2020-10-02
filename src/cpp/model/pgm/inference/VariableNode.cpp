#include "VariableNode.h"

using namespace std;

namespace tomcat {
    namespace model {

        using namespace std;

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        VariableNode::VariableNode(const string& label,
                                   int time_step,
                                   int cardinality)
            : MessageNode(label, time_step), cardinality(cardinality) {}

        VariableNode::~VariableNode() {}

        //----------------------------------------------------------------------
        // Copy & Move constructors/assignments
        //----------------------------------------------------------------------
        VariableNode::VariableNode(const VariableNode& node) {
            this->copy_node(node);
        }

        VariableNode& VariableNode::operator=(const VariableNode& node) {
            this->copy_node(node);
            return *this;
        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void VariableNode::copy_node(const VariableNode& node) {
            MessageNode::copy_node(node);
            this->cardinality = node.cardinality;
            this->data_per_time_slice = node.data_per_time_slice;
        }

        Eigen::MatrixXd VariableNode::get_outward_message_to(
            const shared_ptr<MessageNode>& template_target_node,
            int template_time_step,
            int target_time_step,
            Direction direction) const {

            Eigen::MatrixXd outward_message;
            if (EXISTS(template_time_step, this->data_per_time_slice)) {
                // If there's data for the node, just report the one-hot-encode
                // representation of that data as the message emitted by this
                // node.
                outward_message =
                    this->data_per_time_slice.at(template_time_step);
            }
            else {
                MessageContainer message_container =
                    this->incoming_messages_per_time_slice.at(
                        template_time_step);

                for (const auto& [incoming_node_name, incoming_message] :
                     message_container.node_name_to_messages) {

                    if (incoming_node_name ==
                        MessageNode::get_name(template_target_node->get_label(),
                                              target_time_step)) {
                        continue;
                    }

                    if (outward_message.rows() == 0) {
                        outward_message = incoming_message;
                    }
                    else {
                        outward_message =
                            outward_message.array() * incoming_message.array();
                    }
                }
            }

            // Outliers can result in zero vector probabilities. Adding a noise
            // to generate a uniform distribution after normalization.
            outward_message = outward_message.array() + EPSILON;
            Eigen::VectorXd sum_per_row =
                outward_message.rowwise().sum().array();
            outward_message =
                (outward_message.array().colwise() / sum_per_row.array())
                    .matrix();

            return outward_message;
        }

        bool VariableNode::is_factor() const { return false; }

        Eigen::MatrixXd VariableNode::get_marginal_at(int time_step,
                                                      bool normalized) const {
            Eigen::MatrixXd marginal;

            for (const auto& [incoming_node_name, incoming_message] :
                 this->incoming_messages_per_time_slice.at(time_step)
                     .node_name_to_messages) {

                if (marginal.rows() == 0) {
                    marginal = incoming_message;
                }
                else {
                    marginal = marginal.array() * incoming_message.array();
                }
            }

            if (normalized) {
                // Outliers can result in zero vector probabilities. Adding a
                // noise to generate a uniform distribution after normalization.
                marginal = marginal.array() + EPSILON;
                Eigen::VectorXd sum_per_row = marginal.rowwise().sum().array();
                marginal =
                    (marginal.array().colwise() / sum_per_row.array()).matrix();
            }

            return marginal;
        }

        void VariableNode::set_data_at(int time_step,
                                       const Eigen::VectorXd& data) {
            // Convert each element of the vector to a binary row vector and
            // stack them horizontally;
            Eigen::MatrixXd data_matrix(data.size(), this->cardinality);
            for (int i = 0; i < data.size(); i++) {
                Eigen::VectorXd binary_vector =
                    Eigen::VectorXd::Zero(this->cardinality);
                binary_vector[data[i]] = 1;
                data_matrix.row(i) = move(binary_vector);
            }

            this->data_per_time_slice[time_step] = data_matrix;
        }

        void VariableNode::erase_data_at(int time_step) {
            this->data_per_time_slice.erase(time_step);
        }

        //----------------------------------------------------------------------
        // Getters & Setters
        //----------------------------------------------------------------------
        int VariableNode::get_cardinality() const { return cardinality; }

    } // namespace model
} // namespace tomcat
