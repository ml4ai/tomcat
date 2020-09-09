#include "Estimator.h"

#include <sstream>

namespace tomcat {
    namespace model {

        using namespace std;

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        Estimator::Estimator() {}

        Estimator::Estimator(shared_ptr<DynamicBayesNet> model, int inference_horizon)
            : model(model), inference_horizon(inference_horizon) {}

        Estimator::~Estimator() {}

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void Estimator::copy_estimator(const Estimator& estimator) {
            this->model = estimator.model;
            this->training_data = estimator.training_data;
            this->test_data = estimator.test_data;
            this->nodes_estimates = estimator.nodes_estimates;
            this->inference_horizon = estimator.inference_horizon;
        }

        void Estimator::add_node(const string& node_label,
                                 const Eigen::VectorXd& assignment) {
            NodeEstimates node_estimates;
            node_estimates.label = node_label;
            node_estimates.assignment = assignment;
            this->nodes_estimates.push_back(node_estimates);
        }

        vector<NodeEstimates>
        Estimator::get_estimates_at(int time_step) const {
            vector<NodeEstimates> estimates_per_node;
            estimates_per_node.reserve(this->nodes_estimates.size());

            for (const auto& node_estimate : this->nodes_estimates) {
                if (node_estimate.estimates.cols() <= time_step) {
                    stringstream ss;
                    ss << "The chosen estimator can only calculate estimates "
                          "up to time step "
                       << time_step;
                    throw out_of_range(ss.str());
                }

                NodeEstimates sliced_estimates;
                sliced_estimates.label = node_estimate.label;
                sliced_estimates.assignment = node_estimate.assignment;
                sliced_estimates.estimates =
                    node_estimate.estimates.col(time_step);
                estimates_per_node.push_back(sliced_estimates);
            }

            return estimates_per_node;
        }

        vector<NodeEstimates> Estimator::get_estimates() const {
            vector<NodeEstimates> estimates_per_node;
            estimates_per_node.reserve(this->nodes_estimates.size());

            for (const auto& node_estimate : this->nodes_estimates) {
                NodeEstimates sliced_estimates;
                sliced_estimates.label = node_estimate.label;
                sliced_estimates.assignment = node_estimate.assignment;
                sliced_estimates.estimates = node_estimate.estimates;
                estimates_per_node.push_back(sliced_estimates);
            }

            return estimates_per_node;
        }

        void Estimator::prepare() {
            // Clear estimates so they can be recalculated over the new
            // training data in the next call to the function estimate.
            for (auto& node_estimates : this->nodes_estimates) {
                node_estimates.estimates = Eigen::MatrixXd(0, 0);
            }
        }

        //----------------------------------------------------------------------
        // Getters & Setters
        //----------------------------------------------------------------------
        void Estimator::set_training_data(const EvidenceSet& training_data) {
            this->training_data = training_data;
        }

        int Estimator::get_inference_horizon() const {
            return inference_horizon;
        }
    } // namespace model
} // namespace tomcat
