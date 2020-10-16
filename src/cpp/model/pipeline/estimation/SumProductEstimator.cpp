#include "SumProductEstimator.h"

#include <iostream>

#include <boost/progress.hpp>

#include "pgm/inference/FactorGraph.h"
#include "pgm/inference/VariableNode.h"

namespace tomcat {
    namespace model {

        using namespace std;

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        SumProductEstimator::SumProductEstimator(
            shared_ptr<DynamicBayesNet> model,
            int inference_horizon,
            const std::string& node_label,
            const Eigen::VectorXd& assignment)
            : Estimator(model, inference_horizon, node_label, assignment) {}

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
            this->factor_graph.store_topological_traversal_per_time_step();
        }

        void SumProductEstimator::estimate(const EvidenceSet& new_data) {
            this->estimate_forward_in_time(new_data);

            // The following is only needed in case there's a need for
            // re-estimating things in the past given observations in the
            // future. So far there's no such requirement.
            // this->estimate_backward_in_time(new_data);
        }

        void SumProductEstimator::estimate_forward_in_time(
            const EvidenceSet& new_data) {

            int total_time = this->next_time_step + new_data.get_time_steps();
            cout << "Sum-Product (h = " << this->inference_horizon << ")";
            boost::progress_display progress(total_time);
            for (int t = this->next_time_step; t < total_time; t++) {

                this->compute_forward_messages(this->factor_graph, t, new_data);
                this->compute_backward_messages(
                    this->factor_graph, t, new_data);

                if (this->inference_horizon > 0) {
                    int discrete_assignment = this->estimates.assignment[0];

                    Eigen::VectorXd estimates_in_time_step =
                        this->get_predictions_for(
                            this->estimates.label,
                            t,
                            discrete_assignment,
                            new_data.get_num_data_points());

                    this->add_column_to_estimates(estimates_in_time_step);
                }
                else {
                    Eigen::MatrixXd marginal =
                        this->factor_graph.get_marginal_for(this->estimates.label, t, true);

                    if (marginal.size() == 0) {
                        marginal = Eigen::MatrixXd::Constant(
                            new_data.get_num_data_points(), 1, NO_OBS);
                    }
                    else {
                        if (this->estimates.assignment.size() != 0) {
                            int discrete_assignment = this->estimates.assignment[0];
                            Eigen::VectorXd estimates_in_time_step =
                                marginal.col(discrete_assignment);

                            this->add_column_to_estimates(estimates_in_time_step);
                        }
                        else {
                            for (int col = 0; col < marginal.cols(); col++) {
                                Eigen::VectorXd estimates_in_time_step =
                                    marginal.col(col);
                                this->add_column_to_estimates(estimates_in_time_step, col);
                            }
                        }
                    }
                }

                ++progress;
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
                        //                        cout << MessageNode::get_name(
                        //                                    parent_node->get_label(),
                        //                                    parent_incoming_messages_time_step)
                        //                             << " -> "
                        //                             <<
                        //                             MessageNode::get_name(node->get_label(),
                        //                                                      time_step)
                        //                             << "\n";
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
                    for (const auto& child_node : child_nodes) {

                        // Relative distance in time from the node to the
                        // child. 0 if they are in the same time slice.
                        int time_diff =
                            child_node->get_time_step() - node->get_time_step();

                        // We compute the backward passing constrained to the
                        // fact that we are doing inference up to the time step
                        // being processed, so we do not process child nodes in
                        // a future time step.
                        if (time_diff == 0) {
                            Eigen::MatrixXd message =
                                child_node->get_outward_message_to(
                                    node,
                                    time_step,
                                    time_step,
                                    MessageNode::Direction::backwards);

                            //                            LOG("Backward");
                            //                            cout <<
                            //                            MessageNode::get_name(node->get_label(),
                            //                                                          time_step)
                            //                                 << " -> "
                            //                                 <<
                            //                                 MessageNode::get_name(
                            //                                        child_node->get_label(),
                            //                                        time_step)
                            //
                            //                                 << "\n";
                            //                            LOG(message);
                            //                            LOG("");

                            node->set_incoming_message_from(
                                child_node->get_label(),
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
            Eigen::VectorXd estimated_probabilities;

            for (int h = 1; h <= this->inference_horizon; h++) {
                // Simulate new data coming and compute estimates in a regular
                // way.
                this->next_time_step = time_step + h;
                this->compute_forward_messages(
                    this->factor_graph, time_step + h, horizon_data);
                this->compute_backward_messages(
                    this->factor_graph, time_step + h, horizon_data);

                Eigen::MatrixXd marginal = factor_graph.get_marginal_for(
                    node_label, time_step + h, false);

                if (estimated_probabilities.size() == 0) {
                    estimated_probabilities = marginal.col(opposite_assignment);
                }
                else {
                    estimated_probabilities = estimated_probabilities.array() *
                                marginal.col(opposite_assignment).array();
                }
            }
            // Adjust the time counter back to it's original position.
            this->next_time_step -= (time_step + this->inference_horizon);
            this->factor_graph.erase_incoming_messages_beyond(time_step);

            estimated_probabilities = 1 - estimated_probabilities.array();

            return estimated_probabilities;
        }

        void SumProductEstimator::add_column_to_estimates(
            const Eigen::VectorXd new_column,
            int index) {
            if (this->estimates.estimates.size() < index + 1) {
                // First estimates for a given assignment
                this->estimates.estimates.push_back(Eigen::MatrixXd(0, 0));
                this->estimates.estimates[index] = new_column;
            } else {
                // Append new column to the existing estimates
                int num_rows = this->estimates.estimates[index].rows();
                int num_cols = this->estimates.estimates[index].cols() + 1;
                this->estimates.estimates[index].conservativeResize(num_rows,
                                                              num_cols);
                this->estimates.estimates[index].col(num_cols - 1) = new_column;
            }

        }

        void SumProductEstimator::estimate_backward_in_time(
            const EvidenceSet& new_data) {

            for (int t = this->next_time_step - 1; t > 0; t--) {
                unordered_set<shared_ptr<FactorNode>> transition_factors =
                    this->factor_graph.get_transition_factors_at(t);

                for (auto& factor : transition_factors) {
                    // Pass message from transition factor backward in time, to
                    // it's parents in a different time step.
                    for (auto& [parent_node, transition] :
                         this->factor_graph.get_parents_of(factor, t)) {
                        if (transition) {
                            Eigen::MatrixXd message =
                                factor->get_outward_message_to(
                                    parent_node,
                                    t,
                                    t - 1,
                                    MessageNode::Direction::backwards);

                            parent_node->set_incoming_message_from(
                                factor->get_label(), t, t - 1, message);
                        }
                    }
                }

                // Adjust messages in the previous time slice to account for the
                // new message that was passed backward.
                this->compute_forward_messages(
                    this->factor_graph, t - 1, new_data);
                this->compute_backward_messages(
                    this->factor_graph, t - 1, new_data);
            }
        }

        void SumProductEstimator::get_info(nlohmann::json& json) const {
            json["name"] = this->get_name();
            json["inference_horizon"] = this->inference_horizon;
        }

        string SumProductEstimator::get_name() const { return "sum-product"; }

    } // namespace model
} // namespace tomcat
