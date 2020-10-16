#include "Estimator.h"

#include <sstream>

namespace tomcat {
    namespace model {

        using namespace std;

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        Estimator::Estimator() {}

        Estimator::Estimator(shared_ptr<DynamicBayesNet> model,
                             int inference_horizon,
                             const std::string& node_label,
                             const Eigen::VectorXd& assignment)
            : model(model), inference_horizon(inference_horizon) {

            if (inference_horizon > 0 && assignment.size() == 0) {
                throw TomcatModelException(
                    "An assignment must be given for estimations with "
                    "inference horizon greater than 0.");
            }

            this->estimates.label = node_label;
            this->estimates.assignment = assignment;
            this->cumulative_estimates.label = node_label;
            this->cumulative_estimates.assignment = assignment;
        }

        Estimator::~Estimator() {}

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void Estimator::copy_estimator(const Estimator& estimator) {
            this->model = estimator.model;
            this->training_data = estimator.training_data;
            this->test_data = estimator.test_data;
            this->estimates = estimator.estimates;
            this->cumulative_estimates = estimator.cumulative_estimates;
            this->inference_horizon = estimator.inference_horizon;
        }

        NodeEstimates Estimator::get_estimates_at(int time_step) const {
            if (this->estimates.estimates[0].cols() <= time_step) {
                stringstream ss;
                ss << "The chosen estimator can only calculate estimates "
                      "up to time step "
                   << time_step;
                throw out_of_range(ss.str());
            }

            NodeEstimates sliced_estimates;
            sliced_estimates.label = this->estimates.label;
            sliced_estimates.assignment = this->estimates.assignment;
            for (const auto& estimates_per_assignment :
                 this->estimates.estimates) {
                sliced_estimates.estimates.push_back(
                    estimates_per_assignment.col(time_step));
            }

            return sliced_estimates;
        }

        void Estimator::prepare() {
            // Clear estimates so they can be recalculated over the new
            // training data in the next call to the function estimate.
            this->estimates.estimates.clear();
        }

        void Estimator::keep_estimates() {
            this->cumulative_estimates.estimates.push_back({});
            int i = this->cumulative_estimates.estimates.size() - 1;
            for (const auto& estimate : this->estimates.estimates) {
                this->cumulative_estimates.estimates[i].push_back(estimate);
            }
        }

        void Estimator::clear_estimates() {
            int i = 0;
            this->estimates.estimates.clear();
            this->cumulative_estimates.estimates.clear();
        }

        //----------------------------------------------------------------------
        // Getters & Setters
        //----------------------------------------------------------------------
        NodeEstimates Estimator::get_estimates() const {
            return this->estimates;
        }

        CumulativeNodeEstimates Estimator::get_cumulative_estimates() const {
            return this->cumulative_estimates;
        }

        void Estimator::set_training_data(const EvidenceSet& training_data) {
            this->training_data = training_data;
        }

        int Estimator::get_inference_horizon() const {
            return inference_horizon;
        }

        const shared_ptr<DynamicBayesNet>& Estimator::get_model() const {
            return model;
        }
    } // namespace model
} // namespace tomcat
