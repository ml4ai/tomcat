#include "TrainingFrequencyEstimator.h"

#include <iostream>

namespace tomcat {
    namespace model {

        using namespace std;

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        TrainingFrequencyEstimator::TrainingFrequencyEstimator(
            shared_ptr<DynamicBayesNet> model, int inference_horizon)
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
                            "No training data was provided to the node " +
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

                }
            }
        }

        void TrainingFrequencyEstimator::get_info(nlohmann::json& json) const {
            json["name"] = this->get_name();
            json["inference_horizon"] = this->inference_horizon;
        }

        string TrainingFrequencyEstimator::get_name() const {
            return "training frequency";
        }

    } // namespace model
} // namespace tomcat
