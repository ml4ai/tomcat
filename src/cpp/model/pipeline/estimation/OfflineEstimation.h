#pragma once

#include "../../utils/Definitions.h"

#include "EstimationProcess.h"

namespace tomcat {
    namespace model {

        /**
         * Class responsible for computing estimates for a model in an offline
         * fashion. The test data received from the pipeline is used in batch to
         * compute the estimates.
         */
        class OfflineEstimation : public EstimationProcess {
          public:
            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates an offline estimation process.
             */
            OfflineEstimation();

            ~OfflineEstimation();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------
            OfflineEstimation(const OfflineEstimation& estimation);

            OfflineEstimation& operator=(const OfflineEstimation& estimation);

            OfflineEstimation(OfflineEstimation&&) = default;

            OfflineEstimation& operator=(OfflineEstimation&&) = default;

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------
            void estimate(EvidenceSet test_data) override;

          private:
            //------------------------------------------------------------------
            // Static functions
            //------------------------------------------------------------------

            /**
             * Function executed by a thread responsible for calculating the
             * estimates for a single estimator.
             *
             * @param estimator: estimator
             * @param test_data: data to estimate values over
             */
            static void
            run_estimation_thread(std::shared_ptr<Estimator> estimator,
                                  const EvidenceSet& test_data);
        };

    } // namespace model
} // namespace tomcat
