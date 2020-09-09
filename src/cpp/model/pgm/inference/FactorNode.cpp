#include "FactorNode.h"

using namespace std;

namespace tomcat {
    namespace model {

        using namespace std;

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        FactorNode::FactorNode(const string& label,
                               int time_step,
                               const Eigen::MatrixXd& potential_function,
                               const CPD::TableOrderingMap& ordering_map,
                               const string cpd_main_node_label)
            : MessageNode(compose_label(label), time_step),
              original_potential_function(
                  ordering_map, potential_function, cpd_main_node_label) {

            this->adjust_potential_functions();
        }

        FactorNode::~FactorNode() {}

        //----------------------------------------------------------------------
        // Copy & Move constructors/assignments
        //----------------------------------------------------------------------
        FactorNode::FactorNode(const FactorNode& node) {
            this->copy_node(node);
        }

        FactorNode& FactorNode::operator=(const FactorNode& node) {
            this->copy_node(node);
            return *this;
        }

        //----------------------------------------------------------------------
        // Static functions
        //----------------------------------------------------------------------
        string FactorNode::compose_label(const string& variable_node_label) {
            return "f:" + variable_node_label;
        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
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
                new_function.matrix = move(new_matrix);
                new_function.main_node_label = node_label;
                // Create a new ordering map and replace the parent node's label
                // by the main node name, that happens to be the child of this
                // factor node;
                CPD::TableOrderingMap new_map =
                    this->original_potential_function.ordering_map;
                string prev_main_node_label =
                    this->original_potential_function.main_node_label;
                if (EXISTS(prev_main_node_label, new_map)) {
                    new_function.duplicate_key = prev_main_node_label;
                    // Adds * to the key to differentiate it from the already
                    // existing key with the same label. E.g. transition matrix
                    // between one node with a label and another node with the
                    // same label but in the future.
                    prev_main_node_label =
                        PotentialFunction::get_alternative_key_label(
                            prev_main_node_label);
                }
                auto map_entry = new_map.extract(node_label);
                map_entry.key() = prev_main_node_label;
                new_map.insert(move(map_entry));

                new_function.ordering_map = move(new_map);

                this->node_label_to_rotated_potential_function[node_label] =
                    new_function;
            }
        }

        void FactorNode::copy_node(const FactorNode& node) {
            MessageNode::copy_node(node);
            this->original_potential_function =
                node.original_potential_function;
            this->node_label_to_rotated_potential_function =
                node.node_label_to_rotated_potential_function;
        }

        Eigen::MatrixXd FactorNode::get_outward_message_to(
            const shared_ptr<MessageNode>& template_target_node,
            int template_time_step,
            int target_time_step,
            Direction direction) const {

            PotentialFunction potential_function;
            if (direction == Direction::forward) {
                potential_function = original_potential_function;
            }
            else {
                potential_function =
                    this->node_label_to_rotated_potential_function.at(
                        template_target_node->get_label());
            }

            // To achieve the correct indexing when multiplying incoming
            // messages by the potential function matrix, the messages have to
            // be multiplied following the CPD order defined for parent nodes.
            vector<Eigen::MatrixXd> messages_in_order =
                this->get_incoming_messages_in_order(
                    template_target_node->get_label(),
                    template_time_step,
                    target_time_step,
                    potential_function);

            // Each row in the message matrix contains the message for one data
            // point. We need to perform the following operation individually
            // for each data point before multiplying by the potential function
            // matrix. For instance:
            // m1 = [a, b, c]
            // m2 = [d, e, f]
            // temp_vector = [ad, ae, af, bd, be, bf, cd, ce, cf]
            int num_data_points = messages_in_order[0].rows();
            Eigen::MatrixXd temp_matrix(num_data_points,
                                        potential_function.matrix.rows());
            for (int d = 0; d < num_data_points; d++) {
                Eigen::VectorXd temp_vector = messages_in_order[0].row(d);

                for (int i = 1; i < messages_in_order.size(); i++) {
                    Eigen::MatrixXd cartesian_product =
                        temp_vector * messages_in_order[i].row(d);
                    cartesian_product.transposeInPlace();
                    // Flatten the matrix
                    temp_vector = Eigen::Map<Eigen::VectorXd>(
                        cartesian_product.data(), cartesian_product.size());
                }

                temp_matrix.row(d) = temp_vector;
            }

            // This will marginalize the incoming nodes by summing the rows.
            Eigen::MatrixXd outward_message =
                temp_matrix * potential_function.matrix;

            // Normalize the message
            Eigen::VectorXd sum_per_row = outward_message.rowwise().sum();
            outward_message =
                (outward_message.array().colwise() / sum_per_row.array())
                    .matrix();

            return outward_message;
        }

        vector<Eigen::MatrixXd> FactorNode::get_incoming_messages_in_order(
            const string& ignore_label,
            int template_time_step,
            int target_time_step,
            const PotentialFunction& potential_function) const {

            int num_messages =
                this->incoming_messages_per_time_slice.at(template_time_step)
                    .size();
            vector<Eigen::MatrixXd> messages_in_order(num_messages);
            int added_duplicate_key_order = -1;
            int added_duplicate_key_time_step = -1;

            MessageContainer message_container =
                this->incoming_messages_per_time_slice.at(template_time_step);

            for (const auto& [incoming_node_name, incoming_message] :
                 message_container.node_name_to_messages) {

                if (MessageNode::is_prior(incoming_node_name)) {
                    // No parents. There's only one incoming message.
                    messages_in_order.resize(1);
                    messages_in_order[0] = incoming_message;
                    break;
                }
                else {
                    // Ignore messages that come from a specific node. Because
                    // messages can go forward and backwards in the graph, when
                    // computing the messages that go towards a target node via
                    // this node, the messages that arrive in this node from
                    // that same target have to be ignored.
                    if (incoming_node_name ==
                        MessageNode::get_name(ignore_label, target_time_step)) {
                        messages_in_order.pop_back();
                        continue;
                    }

                    int order = 0;
                    auto [incoming_node_label, incoming_node_time_step] =
                        MessageNode::strip(incoming_node_name);

                    if (potential_function.duplicate_key ==
                        incoming_node_label) {
                        // The potential function matrix is indexed by another
                        // node with the same label. We need to detect the
                        // correct order of this node somehow. We simply use the
                        // order defined for one of the labels and adjust later
                        // by swapping the orders when we process the second
                        // entry with the same label.

                        if (added_duplicate_key_order < 0) {
                            order = potential_function.ordering_map
                                        .at(incoming_node_label)
                                        .order;
                            added_duplicate_key_order = order;
                            added_duplicate_key_time_step =
                                incoming_node_time_step;
                        }
                        else {
                            string alternative_key_label =
                                PotentialFunction::get_alternative_key_label(
                                    incoming_node_label);
                            order = potential_function.ordering_map
                                        .at(alternative_key_label)
                                        .order;
                            if (incoming_node_time_step <
                                added_duplicate_key_time_step) {
                                // This node is in the past with respect to the
                                // previous node with the same key processed.
                                // Therefore, they have to change position as
                                // the node's alternative key has to be in the
                                // future according to how CPDs were defined in
                                // this implementation. An CPD indexing node is
                                // never in the future regarding the CPD's
                                // main node. When CPDs were adjusted in this
                                // factor node, the main node was swapped with
                                // one of the indexing nodes and an alternative
                                // key was created if there was a conflict with
                                // an already existing label. Therefore, nodes
                                // with alternative keys can never be in the
                                // past and we must swap the order with the
                                // previously processed label of the same kind.
                                messages_in_order[order] = messages_in_order
                                    [added_duplicate_key_order];
                                order = added_duplicate_key_order;
                            }
                        }
                    }
                    else {
                        order = potential_function.ordering_map
                                    .at(incoming_node_label)
                                    .order;
                    }

                    messages_in_order[order] = incoming_message;
                }
            }

            return messages_in_order;
        }

        bool FactorNode::is_factor() const { return true; }

    } // namespace model
} // namespace tomcat
