#include "SumProductEstimator.h"

#include <iostream>

#include "../../pgm/inference/FactorGraph.h"
#include "../../pgm/inference/VariableNode.h"

namespace tomcat {
    namespace model {

        using namespace std;

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        SumProductEstimator::SumProductEstimator(
            shared_ptr<DynamicBayesNet> model, int inference_horizon)
            : Estimator(model, inference_horizon) {}

        SumProductEstimator::~SumProductEstimator() {}

        //----------------------------------------------------------------------
        // Copy & Move constructors/assignments
        //----------------------------------------------------------------------
        SumProductEstimator::SumProductEstimator(
            const SumProductEstimator& estimator) {
            Estimator::copy_estimator(estimator);
            this->next_time_step = estimator.next_time_step;
        }

        SumProductEstimator&
        SumProductEstimator::operator=(const SumProductEstimator& estimator) {
            Estimator::copy_estimator(estimator);
            this->next_time_step = estimator.next_time_step;
            return *this;
        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void SumProductEstimator::prepare() {
            Estimator::prepare();
            this->next_time_step = 0;
            this->factor_graph =
                FactorGraph::create_from_unrolled_dbn(*this->model);
        }

        void SumProductEstimator::estimate(EvidenceSet new_data) {
            this->factor_graph.store_topological_traversal_per_time_step();

            for (int t = this->next_time_step;
                 t < this->next_time_step + new_data.get_time_steps();
                 t++) {

                this->compute_forward_messages(this->factor_graph, t, new_data);
                this->compute_backward_messages(
                    this->factor_graph, t, new_data);

                for (auto& estimates : this->nodes_estimates) {
                    Eigen::VectorXd estimates_in_time_step;
                    int discrete_assignment = estimates.assignment[0];

                    if (this->inference_horizon > 0) {
                        estimates_in_time_step = this->get_predictions_for(
                            estimates.label,
                            t,
                            discrete_assignment,
                            new_data.get_num_data_points());
                    }
                    else {
                        // This could be incorporated to the marginal with
                        // horizon but I prefered to let it separate as for
                        // inference, it's possible to retrieve the whole
                        // distribution. Conversely, with horizons this
                        // implementation is limited to find the probability of
                        // a given value in a binary node.
                        Eigen::MatrixXd marginal =
                            factor_graph.get_marginal_for(estimates.label, t);

                        if (marginal.size() == 0) {
                            estimates_in_time_step = Eigen::MatrixXd::Constant(
                                new_data.get_num_data_points(), 1, NO_OBS);
                        }
                        else {
                            estimates_in_time_step =
                                marginal.col(discrete_assignment);
                        }
                    }

                    this->add_column_to_estimates(estimates,
                                                  estimates_in_time_step);
                }
            }

            this->next_time_step += new_data.get_time_steps();
        }

        void SumProductEstimator::compute_forward_messages(
            const FactorGraph& factor_graph,
            int time_step,
            const EvidenceSet& new_data) {

            for (auto& node :
                 factor_graph.get_vertices_topological_order_in(time_step)) {

                vector<pair<shared_ptr<MessageNode>, bool>> parent_nodes =
                    factor_graph.get_parents_of(node, time_step);

                if (!node->is_factor()) {
                    shared_ptr<VariableNode> variable_node =
                        dynamic_pointer_cast<VariableNode>(node);
                    if (new_data.has_data_for(node->get_label())) {
                        // Column of the data matrix that contains data for the
                        // time step being processed.
                        Tensor3 node_data = new_data[node->get_label()];
                        Eigen::VectorXd data_in_time_step = node_data(0, 0).col(
                            time_step - this->next_time_step);
                        variable_node->set_data_at(time_step,
                                                   data_in_time_step);
                    }
                    else {
                        variable_node->erase_data_at(time_step);
                    }
                }

                if (parent_nodes.empty()) {
                    // This vertex is a factor that represents the prior
                    // probability of the factor's child node.
                    int num_rows = max(1, new_data.get_num_data_points());
                    node->set_incoming_message_from(
                        MessageNode::PRIOR_NODE_LABEL,
                        time_step,
                        time_step,
                        Eigen::MatrixXd::Ones(num_rows, 1));
                }
                else {
                    for (const auto& [parent_node, transition] : parent_nodes) {
                        int parent_incoming_messages_time_step = time_step;
                        if (transition) {
                            // If it's a node that links nodes in different time
                            // steps, the messages that arrive to this parent
                            // node comes from the table of messages of the
                            // previous time step.
                            parent_incoming_messages_time_step = time_step - 1;
                        }

                        Eigen::MatrixXd message =
                            parent_node->get_outward_message_to(
                                node,
                                parent_incoming_messages_time_step,
                                time_step,
                                MessageNode::Direction::forward);

//                        LOG("Forward");
//                        LOG(MessageNode::get_name(node->get_label(),
//                                                  time_step));
//                        LOG(message);
//                        LOG("");

                        node->set_incoming_message_from(
                            parent_node->get_label(),
                            parent_incoming_messages_time_step,
                            time_step,
                            message);
                    }
                }
            }
        }

        void SumProductEstimator::compute_backward_messages(
            const FactorGraph& factor_graph,
            int time_step,
            const EvidenceSet& new_data) {

            for (auto& node : factor_graph.get_vertices_topological_order_in(
                     time_step, false)) {

                vector<shared_ptr<MessageNode>> child_nodes =
                    factor_graph.get_children_of(node);

                if (child_nodes.empty() ||
                    (child_nodes.size() == 1 &&
                     child_nodes[0]->get_time_step() - node->get_time_step() >
                         0)) {
                    // This vertex is a leaf in the factor graph for that time
                    // step (it can have a child in the next time step, which
                    // should not be considered at this point). It's single
                    // incoming bottom up message is a vector of ones.
                    // Implemented as a matrix so that messages for multiple
                    // data sets can be processes at once.
                    int num_rows = max(1, new_data.get_num_data_points());
                    int num_cols = dynamic_pointer_cast<VariableNode>(node)
                                       ->get_cardinality();

                    node->set_incoming_message_from(
                        MessageNode::END_NODE_LABEL,
                        time_step,
                        time_step,
                        Eigen::MatrixXd::Ones(num_rows, num_cols));
                }
                else {
                    for (const auto& child_vertex : child_nodes) {

                        // Relative distance in time from the node to the
                        // child. 0 if they are in the same time slice.
                        int time_diff = child_vertex->get_time_step() -
                                        node->get_time_step();

                        // We compute the backward passing constrained to the
                        // fact that we are doing inference up to the time step
                        // being processed, so we do not process child nodes in
                        // a future time step.
                        if (time_diff == 0) {
                            Eigen::MatrixXd message =
                                child_vertex->get_outward_message_to(
                                    node,
                                    time_step,
                                    time_step,
                                    MessageNode::Direction::backwards);

//                            LOG("Backward");
//                            LOG(MessageNode::get_name(node->get_label(),
//                                                      time_step));
//                            LOG(message);
//                            LOG("");

                            node->set_incoming_message_from(
                                child_vertex->get_label(),
                                time_step,
                                time_step,
                                message);
                        }
                    }
                }
            }
        }

        Eigen::VectorXd
        SumProductEstimator::get_predictions_for(const string& node_label,
                                                 int time_step,
                                                 int assignment,
                                                 int num_data_points) {

            // To compute the probability of observing at least one
            // occurence of an assignment in the inference_horizon, we
            // compute the complement of not observing the assignment in
            // any of the time steps in the inference horizon.
            int opposite_assignment = 1 - assignment;
            Eigen::MatrixXd opposite_assignment_matrix =
                Eigen::MatrixXd::Constant(
                    num_data_points, 1, opposite_assignment);
            EvidenceSet horizon_data;
            horizon_data.add_data(node_label,
                                  Tensor3(opposite_assignment_matrix));
            Eigen::VectorXd estimates;

            for (int h = 1; h <= this->inference_horizon; h++) {
                // Simulate new data coming and compute estimates in a regular
                // way.
                this->next_time_step = time_step + h;
                this->compute_forward_messages(
                    this->factor_graph, time_step + h, horizon_data);
                this->compute_backward_messages(
                    this->factor_graph, time_step + h, horizon_data);

                Eigen::MatrixXd marginal =
                    factor_graph.get_marginal_for(node_label, time_step + h);

                if (estimates.size() == 0) {
                    estimates = marginal.col(opposite_assignment);
                }
                else {
                    estimates = estimates.array() *
                                marginal.col(opposite_assignment).array();
                }
            }
            // Adjust the time counter back to it's original position.
            this->next_time_step -= (time_step + this->inference_horizon);
            this->factor_graph.erase_incoming_messages_beyond(time_step);

            estimates = 1 - estimates.array();

            return estimates;
        }

        void SumProductEstimator::add_column_to_estimates(

            NodeEstimates& estimates, const Eigen::VectorXd new_column) {
            if (estimates.estimates.size() == 0) {
                estimates.estimates = new_column;
            }
            else {
                // Append new column to the existing estimates
                int num_rows = estimates.estimates.rows();
                int num_cols = estimates.estimates.cols() + 1;
                estimates.estimates.conservativeResize(num_rows, num_cols);

                estimates.estimates.col(num_cols - 1) = new_column;
            }
        }

        void SumProductEstimator::get_info(nlohmann::json& json) const {
            json["name"] = this->get_name();
            json["inference_horizon"] = this->inference_horizon;
        }

        string SumProductEstimator::get_name() const {
            return "sum-product";
        }

    } // namespace model
} // namespace tomcat
