#include "FactorNode.h"

using namespace std;

namespace tomcat {
    namespace model {

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        FactorNode::FactorNode(const std::string& label,
                               const Eigen::MatrixXd& potential_function,
                               const CPD::TableOrderingMap& ordering_map)
            : ExactInferenceNode(label), potential_function(potential_function),
              ordering_map(ordering_map) {}

        FactorNode::~FactorNode() {}

        //----------------------------------------------------------------------
        // Copy & Move constructors/assignments
        //----------------------------------------------------------------------
        //        FactorNode(const FactorNode& FactorNode) {}
        //
        //        FactorNode& operator=(const FactorNode& FactorNode) {}

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
        FactorNode::get_outward_message_to(const NodeName& node_name,
                                           int source_time_slice) const {
            // The messages have to be multiplied following the CPD order
            // defined for parent nodes to correct indexing.
            Eigen::MatrixXd adjusted_potential_function;
            vector<Eigen::MatrixXd> messages_in_order =
                this->get_incoming_messages_in_order(
                    node_name, source_time_slice, adjusted_potential_function);

            // Each row in the message matrix contains the message for one data
            // point. We need to perform the following operation individually
            // for each data point before multiplying by the potential function
            // matrix.
            int num_data_points = messages_in_order[0].rows();
            Eigen::MatrixXd temp_matrix(
                num_data_points, adjusted_potential_function.rows());
            for (int d = 0; d < num_data_points; d++) {
                Eigen::VectorXd temp_vector(
                    adjusted_potential_function.rows());
                temp_vector = messages_in_order[0].row(d);

                for (int i = 1; i < messages_in_order.size(); i++) {
                    temp_vector =
                        temp_vector * messages_in_order[i].row(d).transpose();
                }

                temp_matrix.row(d) = temp_vector;
            }

            // This will marginalize the incoming nodes by summing the rows.
            Eigen::MatrixXd outward_message =
                temp_matrix * adjusted_potential_function;

            return outward_message;
        }

        std::vector<Eigen::MatrixXd> FactorNode::get_incoming_messages_in_order(
            const NodeName& excluding_node_name,
            int target_time_slice,
            Eigen::MatrixXd& adjusted_potential_function) const {

            vector<Eigen::MatrixXd> messages_in_order(
                this->incoming_messages_per_time_slice.size());

            for (const auto& [source_node_name, incoming_message] :
                 this->incoming_messages_per_time_slice.at(target_time_slice)) {

                if (source_node_name.label.empty()) {
                    // The factor is a prior
                    messages_in_order[0] = incoming_message;
                    exit;
                }
                else {
                    if (excluding_node_name == source_node_name) {
                        continue;
                    }

                    int order =
                        this->ordering_map.at(source_node_name.label).order;
                    messages_in_order[order] = incoming_message;
                }
            }

            adjusted_potential_function = this->potential_function;
            return messages_in_order;
        }

        //----------------------------------------------------------------------
        // Getters & Setters
        //----------------------------------------------------------------------

    } // namespace model
} // namespace tomcat
