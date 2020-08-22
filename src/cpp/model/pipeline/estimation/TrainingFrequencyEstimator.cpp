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

        TrainingFrequencyEstimator& TrainingFrequencyEstimator::operator=(const TrainingFrequencyEstimator& estimator) {
            this->copy_estimator(estimator);
            return *this;
        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void TrainingFrequencyEstimator::estimate(EvidenceSet new_data) {
            for (auto& node_estimates : this->nodes_estimates) {
                if (node_estimates.estimates.is_empty()) {
                    if (!this->training_data.has_data_for(
                            node_estimates.label)) {
                        throw TomcatModelException(
                            "No data was provided to the node " +
                            node_estimates.label);
                    }

                    // We repeat the frequencies for all of the data points in
                    // the test set.
                    Tensor3 data =
                        this->training_data[node_estimates.label].mean(1);
                    node_estimates.estimates =
                        data.repeat(new_data.get_num_data_points(), 1);
                }
            }
        }

        void TrainingFrequencyEstimator::set_training_data(const EvidenceSet& training_data) {
            Estimator::set_training_data(training_data);

            // Clear estimates so they can be recalculated over the new training
            // data in the next call to the function estimate.
            for (auto& node_estimates : this->nodes_estimates) {
                node_estimates.estimates.clear();
            }
        }

        void TrainingFrequencyEstimator::get_info(nlohmann::json& json) const {
            json["name"] = "training frequency";
            json["inference_horizon"] = this->inference_horizon;
        }

    } // namespace model
} // namespace tomcat
