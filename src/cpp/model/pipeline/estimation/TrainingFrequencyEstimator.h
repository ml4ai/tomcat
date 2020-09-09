#pragma once

#include <unordered_set>

#include "../../utils/Definitions.h"

#include "Estimator.h"

namespace tomcat {
    namespace model {

        /**
         * This estimator is based on the relative frequencies of the
         * observations over the training data for each time step on an unrolled
         * DBN. For instance, the probability of observing a value x in time
         * step t for a given node will be proportional to the number of times
         * the value x is observed in the training data for that node at time t.
         */
        class TrainingFrequencyEstimator : public Estimator {
          public:
            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates an instance of a baseline estimator.
             *
             * @param model: DBN
             * @param inference_horizon: how many time steps in the future
             * estimations are going to be computed for
             */
            TrainingFrequencyEstimator(std::shared_ptr<DynamicBayesNet> model,
                              int inference_horizon);

            ~TrainingFrequencyEstimator();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------
            TrainingFrequencyEstimator(const TrainingFrequencyEstimator& estimator);

            TrainingFrequencyEstimator& operator=(const TrainingFrequencyEstimator& estimator);

            TrainingFrequencyEstimator(TrainingFrequencyEstimator&&) = default;

            TrainingFrequencyEstimator& operator=(TrainingFrequencyEstimator&&) = default;

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------
            void estimate(EvidenceSet new_data) override;

            void get_info(nlohmann::json& json) const override;

            std::string get_name() const override;

        };

    } // namespace model
} // namespace tomcat
