#include "SumProductEstimator.h"

#include <iostream>

#include "../../pgm/inference/FactorGraph.h"
#include "../../pgm/inference/FactorNode.h"
#include "../../pgm/inference/NonFactorNode.h"

namespace tomcat {
    namespace model {

        using namespace std;
        using NodeName = ExactInferenceNode::NodeName;

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
            this->copy_estimator(estimator);
        }

        SumProductEstimator&
        SumProductEstimator::operator=(const SumProductEstimator& estimator) {
            this->copy_estimator(estimator);
            return *this;
        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void SumProductEstimator::estimate(EvidenceSet new_data) {
            this->factor_graph.compute_topological_traversal_per_time_slice();

            for (int t = this->next_time_step;
                 t < this->next_time_step + new_data.get_time_steps();
                 t++) {

                this->compute_forward_messages(this->factor_graph, t, new_data);
                this->compute_backward_messages(
                    this->factor_graph, t, new_data);
            }

            //            for (int t = this->next_time_step +
            //            new_data.get_time_steps() - 1;
            //                 t >= 0;
            //                 t--) {
            //
            //                this->compute_backward_messages(
            //                    this->factor_graph, t, new_data);
            //            }

            //            Eigen::MatrixXd marginal =
            //                factor_graph.get_marginal_for({"S_1", 1});
            //            LOG(marginal);

            for (int t = this->next_time_step;
                 t < this->next_time_step + new_data.get_time_steps();
                 t++) {
                for (auto& estimates : this->nodes_estimates) {
                    //                    int num_cols =
                    //                        this->next_time_step +
                    //                        new_data.get_time_steps() - 1;
                    //                    Eigen::MatrixXd
                    //                    new_estimates(new_data.number);

                    stringstream ss;

                    ss << estimates.label << min(2, t);
                    Eigen::MatrixXd marginal =
                        factor_graph.get_marginal_for({ss.str(), t});
                    if (marginal.size() == 0) {
                        estimates.estimates = Eigen::MatrixXd::Constant(
                            new_data.get_num_data_points(), 1, NO_OBS);
                    }
                    else {
                        // In this implementation, sum-product will only be
                        // executed with discrete one-dimensional variables, so
                        // the assignment vector should only contain one value
                        // which is the discrete state of the node.
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

                        LOG(estimates.estimates);
                    }
                }
            }

            this->next_time_step += new_data.get_time_steps();
        }

        void SumProductEstimator::compute_forward_messages(
            const FactorGraph& factor_graph,
            int time_step,
            const EvidenceSet& new_data) {

            for (auto& vertex :
                 factor_graph.get_vertices_topological_order(time_step)) {

                NodeName node_name(vertex.node->get_label(), time_step);
                vector<FactorGraph::VertexData> parent_vertices =
                    factor_graph.get_parents_of(node_name);

                if (!vertex.factor &&
                    new_data.has_data_for(vertex.node->get_original_label())) {
                    // Column of the data matrix that contains data for the time
                    // step being processed.
                    Tensor3 node_data =
                        new_data[vertex.node->get_original_label()];
                    Eigen::VectorXd data_in_time_step =
                        node_data(0, 0).col(time_step - this->next_time_step);
                    dynamic_pointer_cast<NonFactorNode>(vertex.node)
                        ->set_data_at(time_step, data_in_time_step);
                }

                if (parent_vertices.empty()) {
                    // This vertex is a factor that represents the prior
                    // probability of the factor's child node.
                    int num_rows = max(1, new_data.get_num_data_points());
                    NodeName parent_name("", time_step);
                    vertex.node->set_incoming_message_from(
                        parent_name,
                        time_step,
                        Eigen::MatrixXd::Ones(num_rows, 1));
                }
                else {
                    for (const auto& parent_vertex : parent_vertices) {

                        // Relative distance in time from the parent to the
                        // child. 0 if they are in the same time slice.
                        int time_diff =
                            vertex.time_step - parent_vertex.time_step;

                        Eigen::MatrixXd message =
                            parent_vertex.node->get_outward_message_to(
                                node_name,
                                vertex.time_step - time_diff,
                                time_step);

                        LOG(message);
                        // Normalize the message
                        Eigen::VectorXd sum_per_row = message.rowwise().sum();
                        message =
                            (message.array().colwise() / sum_per_row.array())
                                .matrix();
                        LOG(message);

                        NodeName parent_name(parent_vertex.node->get_label(),
                                             time_step - time_diff);
                        vertex.node->set_incoming_message_from(
                            parent_name, time_step, message);
                    }
                }
            }
        }

        void SumProductEstimator::compute_backward_messages(
            const FactorGraph& factor_graph,
            int time_step,
            const EvidenceSet& new_data) {

            for (auto& vertex : factor_graph.get_vertices_topological_order(
                     time_step, false)) {

                NodeName node_name(vertex.node->get_label(), time_step);
                vector<FactorGraph::VertexData> child_vertices =
                    factor_graph.get_children_of(node_name);

                if (child_vertices.empty()) {
                    // This vertex is a leaf in the factor graph. It's single
                    // incoming bottom up message is a vector of ones.
                    // Implemented as a matrix so that messages for multiple
                    // data points can be processes at once.
                    int num_rows = max(1, new_data.get_num_data_points());
                    int num_cols =
                        dynamic_pointer_cast<NonFactorNode>(vertex.node)
                            ->get_cardinality();
                    NodeName child_name("end_factor", time_step);
                    vertex.node->set_incoming_message_from(
                        child_name,
                        time_step,
                        Eigen::MatrixXd::Ones(num_rows, num_cols));
                }
                else {
                    for (const auto& child_vertex : child_vertices) {

                        // Relative distance in time from the node to the
                        // child. 0 if they are in the same time slice.
                        int time_diff =
                            child_vertex.time_step - vertex.time_step;

                        // We compute the backward passing contrained to the
                        // fact that we are doing inference up to the time step
                        // being processed, so we do not process child nodes in
                        // a future time step.
                        if (time_diff == 0) {
                            Eigen::MatrixXd message =
                                child_vertex.node->get_outward_message_to(
                                    node_name,
                                    vertex.time_step + time_diff,
                                    time_step);

                            LOG(message);
                            // Normalize the message
                            Eigen::VectorXd sum_per_row =
                                message.rowwise().sum();
                            message = (message.array().colwise() /
                                       sum_per_row.array())
                                          .matrix();
                            LOG(message);

                            NodeName child_name(child_vertex.node->get_label(),
                                                time_step + time_diff);
                            vertex.node->set_incoming_message_from(
                                child_name, time_step, message);
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
