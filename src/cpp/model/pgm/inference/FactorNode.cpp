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
              ordering_map(ordering_map) {
            this->fill_rotated_potential_functions();
        }

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
                                           int source_time_slice,
                                           int inference_time_step) const {
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
            Eigen::MatrixXd temp_matrix(num_data_points,
                                        adjusted_potential_function.rows());
            for (int d = 0; d < num_data_points; d++) {
                Eigen::VectorXd temp_vector = messages_in_order[0].row(d);

                for (int i = 1; i < messages_in_order.size(); i++) {
                    Eigen::MatrixXd cartesian_product =
                        temp_vector * messages_in_order[i].row(d);
                    cartesian_product.transposeInPlace();
                    temp_vector = Eigen::Map<Eigen::VectorXd>(
                        cartesian_product.data(), cartesian_product.size());
                }

                temp_matrix.row(d) = temp_vector;
            }

            // This will marginalize the incoming nodes by summing the rows.
            LOG(temp_matrix);
            LOG(adjusted_potential_function);
            Eigen::MatrixXd outward_message =
                temp_matrix * adjusted_potential_function;

            return outward_message;
        }

        std::vector<Eigen::MatrixXd> FactorNode::get_incoming_messages_in_order(
            const NodeName& excluding_node_name,
            int target_time_slice,
            Eigen::MatrixXd& adjusted_potential_function) const {

            bool adjust_potential_function = false;
            vector<Eigen::MatrixXd> messages_in_order(
                this->incoming_messages_per_time_slice.at(target_time_slice)
                    .size());

            for (const auto& [source_node_name, incoming_message] :
                 this->incoming_messages_per_time_slice.at(target_time_slice)) {

                if (source_node_name.label.empty()) {
                    // The factor is a prior
                    messages_in_order[0] = incoming_message;
                    break;
                }
                else {
                    if (excluding_node_name == source_node_name) {
                        messages_in_order.pop_back();
                        continue;
                    }

                    int order = 0;
                    if (!EXISTS(source_node_name.label, this->ordering_map)) {
                        // Source node is a child in the CPD. So we need to
                        // pivot the CPD table by swapping the excluding node
                        // position with the source node one.
                        auto ordering =
                            this->ordering_map.at(excluding_node_name.label);
                        order = ordering.order;
                        adjust_potential_function = true;
                    }
                    else {
                        order =
                            this->ordering_map.at(source_node_name.label).order;
                    }

                    messages_in_order[order] = incoming_message;
                }
            }

            if (adjust_potential_function) {
                int index =
                    this->ordering_map.at(excluding_node_name.label).order;
                adjusted_potential_function =
                    this->rotated_potential_function[index];
            }
            else {
                adjusted_potential_function = this->potential_function;
            }

            return messages_in_order;
        }

        void FactorNode::fill_rotated_potential_functions() {

            int num_rows = this->potential_function.rows();
            int num_cols = this->potential_function.cols();

            this->rotated_potential_function =
                vector<Eigen::MatrixXd>(this->ordering_map.size());

            for (const auto& [node_label, indexing] : this->ordering_map) {
                int block_rows = indexing.right_cumulative_cardinality;
                int block_size = block_rows * num_cols;
                int num_blocks = num_rows / block_rows;

                // Eigen::VectorXd long_vector(num_rows * num_cols);
                int new_num_rows = (num_rows * num_cols) / indexing.cardinality;
                int new_num_cols = indexing.cardinality;
                Eigen::MatrixXd new_matrix(new_num_rows, new_num_cols);
                int row = 0;
                int col = 0;
                for (int b = 0; b < num_blocks; b++) {
                    Eigen::MatrixXd block = this->potential_function.block(
                        b * block_rows, 0, block_rows, num_cols);
                    Eigen::VectorXd vector =
                        Eigen::Map<Eigen::VectorXd>(block.data(), block.size());
                    // long_vector.segment(b * block_size, block_size) = vector;

                    // Stack the vector column-wise in the new matrix.
                    new_matrix.block(row * block_size, col, block_size, 1) =
                        vector;
                    col = (col + 1) % new_num_cols;
                    if (col == 0) {
                        row++;
                    }
                }

                int axis = indexing.order;
                //                int new_num_rows = (num_rows * num_cols) /
                //                indexing.cardinality; int new_num_cols =
                //                indexing.cardinality;
                //                Eigen::Map<Eigen::MatrixXd> new_matrix(
                //                    long_vector.data(), new_num_rows,
                //                    new_num_cols);
                LOG(new_matrix);
                this->rotated_potential_function[axis] = std::move(new_matrix);
            }
        }

        void
        FactorNode::replace_cpd_ordering_label(const std::string& current_label,
                                               const std::string& new_label) {
            auto ordering = this->ordering_map.extract(current_label);
            ordering.key() = new_label;
            this->ordering_map.insert(move(ordering));
        }

        //----------------------------------------------------------------------
        // Getters & Setters
        //----------------------------------------------------------------------

    } // namespace model
} // namespace tomcat
