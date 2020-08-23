#include "TrainingFrequencyEstimator.h"

#include <iostream>

namespace tomcat {
    namespace model {

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        TrainingFrequencyEstimator::TrainingFrequencyEstimator(
            std::shared_ptr<DynamicBayesNet> model, int inference_horizon)
            : Estimator(model, inference_horizon) {}

        TrainingFrequencyEstimator::~TrainingFrequencyEstimator() {}

        //----------------------------------------------------------------------
        // Copy & Move constructors/assignments
        //----------------------------------------------------------------------
        TrainingFrequencyEstimator::TrainingFrequencyEstimator(
            const TrainingFrequencyEstimator& estimator) {
            this->copy_estimator(estimator);
        }

        TrainingFrequencyEstimator& TrainingFrequencyEstimator::operator=(
            const TrainingFrequencyEstimator& estimator) {
            this->copy_estimator(estimator);
            return *this;
        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void TrainingFrequencyEstimator::estimate(EvidenceSet new_data) {
            for (auto& node_estimates : this->nodes_estimates) {
                if (node_estimates.estimates.size() == 0) {
                    if (!this->training_data.has_data_for(
                            node_estimates.label)) {
                        throw TomcatModelException(
                            "No data was provided to the node " +
                            node_estimates.label);
                    }

                    Eigen::MatrixXd logical_data_in_horizon =
                        this->training_data.get_observations_in_window_for(
                            node_estimates.label,
                            node_estimates.assignment,
                            this->inference_horizon);

                    Eigen::MatrixXd estimates(new_data.get_num_data_points(),
                                              logical_data_in_horizon.cols());
                    estimates.row(0) = logical_data_in_horizon.colwise().mean();
                    estimates = estimates.row(0).replicate(
                        new_data.get_num_data_points(), 1);

                    node_estimates.estimates = estimates;

                    LOG(this->training_data[node_estimates.label]);
                    LOG(estimates);
                }
            }
        }

        void TrainingFrequencyEstimator::set_training_data(
            const EvidenceSet& training_data) {
            Estimator::set_training_data(training_data);

            // Clear estimates so they can be recalculated over the new
            // training data in the next call to the function estimate.
            for (auto& node_estimates : this->nodes_estimates) {
                node_estimates.estimates = Eigen::MatrixXd(0, 0);
            }
        }

        void TrainingFrequencyEstimator::get_info(nlohmann::json& json) const {
            json["name"] = "training frequency";
            json["inference_horizon"] = this->inference_horizon;
        }

    } // namespace model
} // namespace tomcat
