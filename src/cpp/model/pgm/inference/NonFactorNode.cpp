#include "NonFactorNode.h"

using namespace std;

namespace tomcat {
    namespace model {

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        NonFactorNode::NonFactorNode(const std::string& label, int cardinality)
            : ExactInferenceNode(label), cardinality(cardinality) {}

        NonFactorNode::~NonFactorNode() {}

        //----------------------------------------------------------------------
        // Copy & Move constructors/assignments
        //----------------------------------------------------------------------
        //        NonFactorNode(const NonFactorNode& NonFactorNode) {}
        //
        //        NonFactorNode& operator=(const NonFactorNode& NonFactorNode)
        //        {}

        //----------------------------------------------------------------------
        // Operator overload
        //----------------------------------------------------------------------

        //----------------------------------------------------------------------
        // Static functions
        //----------------------------------------------------------------------

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        Eigen::MatrixXd
        NonFactorNode::get_outward_message_to(const NodeName& node_name,
                                              int source_time_slice,
                                              int inference_time_step) const {

            Eigen::MatrixXd outward_message;
            if (EXISTS(source_time_slice, this->data_per_time_slice)) {
                outward_message = this->data_per_time_slice.at(inference_time_step);
            } else {
                for (const auto& [source_node_name, incoming_message] :
                     this->incoming_messages_per_time_slice.at(
                         source_time_slice)) {

                    if (source_node_name == node_name) {
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

            return outward_message;
        }

        Eigen::MatrixXd NonFactorNode::get_marginal_at(int time_step) const {
            Eigen::MatrixXd marginal;

            for (const auto& [incoming_node_name, incoming_message] :
                 this->incoming_messages_per_time_slice.at(time_step)) {

                if (marginal.rows() == 0) {
                    marginal = incoming_message;
                }
                else {
                    marginal =
                        marginal.array() * incoming_message.array();
                }
            }

            return marginal;
        }

        void NonFactorNode::set_data_at(int time_step,
                                             const Eigen::VectorXd& data) {
            // Convert each element of the vector to a binary vector and stack
            // them horizontally;
            Eigen::MatrixXd data_matrix(data.size(), this->cardinality);
            for (int i = 0; i < data.size(); i++) {
                Eigen::VectorXd binary_vector = Eigen::VectorXd::Zero(this->cardinality);
                binary_vector[data[i]] = 1;
                data_matrix.row(i) = std::move(binary_vector);
            }

            this->data_per_time_slice[time_step] = data_matrix;
        }

        //----------------------------------------------------------------------
        // Getters & Setters
        //----------------------------------------------------------------------
        int NonFactorNode::get_cardinality() const { return cardinality; }

    } // namespace model
} // namespace tomcat
