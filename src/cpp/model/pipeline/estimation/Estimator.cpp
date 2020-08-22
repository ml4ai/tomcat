#include "Estimator.h"

namespace tomcat {
    namespace model {

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        Estimator::Estimator() {}

        Estimator::Estimator(std::shared_ptr<DynamicBayesNet> model,
                             int inference_horizon)
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
            this->last_processed_time_step = estimator.last_processed_time_step;
            this->inference_horizon = estimator.inference_horizon;
        }

        void Estimator::add_node(const std::string& node_label,
                                 const Eigen::VectorXd& assignment) {
            NodeEstimates node_estimates;
            node_estimates.label = node_label;
            node_estimates.assignment = assignment;
            this->nodes_estimates.push_back(node_estimates);
        }

        std::vector<NodeEstimates> Estimator::get_last_estimates(int initial_time_step) const {
            std::vector<NodeEstimates> last_estimates;
            last_estimates.reserve(this->nodes_estimates.size());

            for (const auto& node_estimate : this->nodes_estimates) {
                NodeEstimates sliced_estimates;
                sliced_estimates.label = node_estimate.label;
                sliced_estimates.assignment = node_estimate.assignment;
                sliced_estimates.estimates = node_estimate.estimates.slice(
                        initial_time_step, Tensor3::ALL, 2);
                last_estimates.push_back(sliced_estimates);
            }

            return last_estimates;
        }

        //----------------------------------------------------------------------
        // Getters & Setters
        //----------------------------------------------------------------------
        void Estimator::set_training_data(const EvidenceSet& training_data) {
            this->training_data = training_data;
        }

    } // namespace model
} // namespace tomcat
