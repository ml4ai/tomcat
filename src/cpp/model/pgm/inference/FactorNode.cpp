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
            : ExactInferenceNode(label),
              original_potential_function(ordering_map, potential_function) {
            this->adjust_potential_functions();
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
        string
        FactorNode::get_factor_label_for_node(const string& non_factor_label) {
            return "f:" + non_factor_label;
        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        string FactorNode::get_child_non_factor_label() const {
            return this->label.substr(2, this->label.size() - 2);
        }

        Eigen::MatrixXd
        FactorNode::get_outward_message_to(const NodeName& node_name,
                                           int inference_time_slice,
                                           Direction direction) const {
            // The messages have to be multiplied following the CPD order
            // defined for parent nodes to correct indexing.
            PotentialFunction potential_function;
            if (direction == Direction::forward) {
                potential_function = original_potential_function;
            }
            else {
                potential_function =
                    this->node_label_to_rotated_potential_function.at(
                        node_name.label);
            }

            vector<Eigen::MatrixXd> messages_in_order =
                this->get_incoming_messages_in_order(
                    node_name, inference_time_slice, potential_function);

            // Each row in the message matrix contains the message for one data
            // point. We need to perform the following operation individually
            // for each data point before multiplying by the potential function
            // matrix.
            int num_data_points = messages_in_order[0].rows();
            Eigen::MatrixXd temp_matrix(num_data_points,
                                        potential_function.matrix.rows());
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
            Eigen::MatrixXd outward_message =
                temp_matrix * potential_function.matrix;

            // Normalize the message
            Eigen::VectorXd sum_per_row =
                outward_message.rowwise().sum();
            outward_message = (outward_message.array().colwise() /
                       sum_per_row.array())
                .matrix();

            return outward_message;
        }

        vector<Eigen::MatrixXd> FactorNode::get_incoming_messages_in_order(
            const NodeName& ignore_node_name,
            int inference_time_step,
            const PotentialFunction& potential_function) const {

            int num_messages = this->incoming_messages_per_time_slice
                                   .at(inference_time_step)
                                   .size();
            vector<Eigen::MatrixXd> messages_in_order(num_messages);
            int added_duplicate_key_order = -1;
            int added_duplicate_key_time_step = -1;

            for (const auto& [source_node_name, incoming_message] :
                 this->incoming_messages_per_time_slice.at(
                     inference_time_step)) {

                if (source_node_name.label.empty()) {
                    // The factor is a prior
                    messages_in_order[0] = incoming_message;
                    break;
                }
                else {
                    if (ignore_node_name == source_node_name) {
                        messages_in_order.pop_back();
                        continue;
                    }

                    int order = 0;
                    if (potential_function.duplicate_key ==
                        source_node_name.label) {
                        if (added_duplicate_key_order < 0) {
                            order = potential_function.ordering_map
                                        .at(source_node_name.label)
                                        .order;
                            added_duplicate_key_order = order;
                            added_duplicate_key_time_step =
                                source_node_name.time_step;
                        }
                        else {
                            string alternative_key_label =
                                source_node_name.label + "*";
                            order = potential_function.ordering_map
                                        .at(alternative_key_label)
                                        .order;
                            if (source_node_name.time_step <
                                added_duplicate_key_time_step) {
                                // This node is in the past with respect to the
                                // previous node with the same key inserted.
                                // Therefore, they have to change position as
                                // Node* has to be in the future by the way CPDs
                                // were defined. An CPD indexing node is always
                                // in the past with regarding the CPD's main
                                // node. When CPDs were adjusted in this factor
                                // node, the main node was swapped with one of
                                // the inxeding nodes and appended * if there
                                // was a conflict with an already existing
                                // label.
                                messages_in_order[order] = messages_in_order
                                    [added_duplicate_key_order];
                                order = added_duplicate_key_order;
                            }
                        }
                    }
                    else {
                        order = potential_function.ordering_map
                                    .at(source_node_name.label)
                                    .order;
                    }

                    messages_in_order[order] = incoming_message;
                }
            }

            return messages_in_order;
        }

        void FactorNode::adjust_potential_functions() {
            int num_rows = this->original_potential_function.matrix.rows();
            int num_cols = this->original_potential_function.matrix.cols();

            for (const auto& [node_label, ordering] :
                 this->original_potential_function.ordering_map) {
                int block_rows = ordering.right_cumulative_cardinality;
                int block_size = block_rows * num_cols;
                int num_blocks = num_rows / block_rows;

                int new_num_rows = (num_rows * num_cols) / ordering.cardinality;
                int new_num_cols = ordering.cardinality;
                Eigen::MatrixXd new_matrix(new_num_rows, new_num_cols);
                int row = 0;
                int col = 0;
                for (int b = 0; b < num_blocks; b++) {
                    Eigen::MatrixXd block =
                        this->original_potential_function.matrix.block(
                            b * block_rows, 0, block_rows, num_cols);
                    Eigen::VectorXd vector =
                        Eigen::Map<Eigen::VectorXd>(block.data(), block.size());

                    // Stack the vector column-wise in the new matrix.
                    new_matrix.block(row * block_size, col, block_size, 1) =
                        vector;
                    col = (col + 1) % new_num_cols;
                    if (col == 0) {
                        row++;
                    }
                }

                PotentialFunction new_function;
                new_function.matrix = std::move(new_matrix);
                // Create a new ordering map and replace the parent node's label
                // by the main node name, that happens to be the child of this
                // factor node;
                CPD::TableOrderingMap new_map =
                    this->original_potential_function.ordering_map;
                string moved_node_label = this->get_child_non_factor_label();
                if (EXISTS(moved_node_label, new_map)) {
                    new_function.duplicate_key = moved_node_label;
                    // Adds * to the key to differentiate it from the already
                    // existing key with the same label. E.g. transition matrix
                    // between one node with a label and another node with the
                    // same label but in the future.
                    moved_node_label = moved_node_label + "*";
                }
                auto map_entry = new_map.extract(node_label);
                map_entry.key() = moved_node_label;
                new_map.insert(move(map_entry));

                new_function.ordering_map = std::move(new_map);

                this->node_label_to_rotated_potential_function[node_label] =
                    new_function;
            }
        }

        //        void
        //        FactorNode::replace_cpd_ordering_label(const std::string&
        //        current_label,
        //                                               const std::string&
        //                                               new_label) {
        //            auto ordering = this->ordering_map.extract(current_label);
        //            ordering.key() = new_label;
        //            this->ordering_map.insert(move(ordering));
        //        }

        //----------------------------------------------------------------------
        // Getters & Setters
        //----------------------------------------------------------------------

    } // namespace model
} // namespace tomcat
