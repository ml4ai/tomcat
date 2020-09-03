#include "SumProductEstimator.h"

#include <iostream>

#include "../../pgm/inference/FactorGraph.h"
#include "../../pgm/inference/FactorNode.h"
#include "../../pgm/inference/VariableNode.h"

namespace tomcat {
    namespace model {

        using namespace std;

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        SumProductEstimator::SumProductEstimator(
            std::shared_ptr<DynamicBayesNet> model, int inference_horizon)
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
        void SumProductEstimator::estimate(EvidenceSet new_data) {
            this->factor_graph.store_topological_traversal_per_time_step();

            for (int t = this->next_time_step;
                 t < this->next_time_step + new_data.get_time_steps();
                 t++) {

                this->compute_forward_messages(this->factor_graph, t, new_data);
                this->compute_backward_messages(
                    this->factor_graph, t, new_data);

                for (auto& estimates : this->nodes_estimates) {
                    Eigen::MatrixXd marginal =
                        factor_graph.get_marginal_for(estimates.label, t);
                    if (marginal.size() == 0) {
                        estimates.estimates = Eigen::MatrixXd::Constant(
                            new_data.get_num_data_points(), 1, NO_OBS);
                    }
                    else {
                        int discrete_assignment = estimates.assignment[0];
                        if (estimates.estimates.size() == 0) {
                            estimates.estimates =
                                marginal.col(discrete_assignment);
                        }
                        else {
                            int num_rows = estimates.estimates.rows();
                            int num_cols = estimates.estimates.cols() + 1;
                            estimates.estimates.conservativeResize(num_rows,
                                                                   num_cols);

                            estimates.estimates.col(num_cols - 1) =
                                marginal.col(discrete_assignment);
                        }
                    }
                }
            }

            for (auto& estimates : this->nodes_estimates) {
                std::cout << estimates.label << " = " << estimates.assignment
                          << std::endl;
                LOG(estimates.estimates);
                LOG("");
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

                if (!node->is_factor() &&
                    new_data.has_data_for(node->get_label())) {
                    // Column of the data matrix that contains data for the time
                    // step being processed.
                    Tensor3 node_data = new_data[node->get_label()];
                    Eigen::VectorXd data_in_time_step =
                        node_data(0, 0).col(time_step - this->next_time_step);
                    dynamic_pointer_cast<VariableNode>(node)->set_data_at(
                        time_step - this->next_time_step, data_in_time_step);
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
                                MessageNode::Direction::forward);

                        LOG("Forward");
                        LOG(node->get_name());
                        LOG(message);
                        LOG("");

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
                                    MessageNode::Direction::backwards);

                            LOG("Backward");
                            LOG(node->get_name());
                            LOG(message);
                            LOG("");

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

        void SumProductEstimator::get_info(nlohmann::json& json) const {
            json["name"] = this->get_name();
            json["inference_horizon"] = this->inference_horizon;
        }

        std::string SumProductEstimator::get_name() const {
            return "sum-product";
        }

    } // namespace model
} // namespace tomcat
