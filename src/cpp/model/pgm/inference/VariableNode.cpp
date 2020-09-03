#include "VariableNode.h"

using namespace std;

namespace tomcat {
    namespace model {

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        VariableNode::VariableNode(const std::string& label,
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
            const std::shared_ptr<MessageNode>& template_target_node,
            int target_time_step,
            Direction direction) const {

            Eigen::MatrixXd outward_message;
            if (EXISTS(target_time_step, this->data_per_time_slice)) {
                outward_message =
                    this->data_per_time_slice.at(target_time_step);
            }
            else {
                MessageContainer message_container =
                    this->incoming_messages_per_time_slice.at(target_time_step);

                for (const auto& [incoming_node_name, incoming_message] :
                     message_container.node_name_to_messages) {

                    if (template_target_node->get_name() == incoming_node_name) {
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

            Eigen::VectorXd sum_per_row = outward_message.rowwise().sum();
            outward_message =
                (outward_message.array().colwise() / sum_per_row.array())
                    .matrix();

            return outward_message;
        }

        bool VariableNode::is_factor() const { return false; }

        Eigen::MatrixXd VariableNode::get_marginal_at(int time_step) const {
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

            return marginal;
        }

        void VariableNode::set_data_at(int time_step,
                                       const Eigen::VectorXd& data) {
            // Convert each element of the vector to a binary vector and stack
            // them horizontally;
            Eigen::MatrixXd data_matrix(data.size(), this->cardinality);
            for (int i = 0; i < data.size(); i++) {
                Eigen::VectorXd binary_vector =
                    Eigen::VectorXd::Zero(this->cardinality);
                binary_vector[data[i]] = 1;
                data_matrix.row(i) = std::move(binary_vector);
            }

            this->data_per_time_slice[time_step] = data_matrix;
        }

        //----------------------------------------------------------------------
        // Getters & Setters
        //----------------------------------------------------------------------
        int VariableNode::get_cardinality() const { return cardinality; }

    } // namespace model
} // namespace tomcat
