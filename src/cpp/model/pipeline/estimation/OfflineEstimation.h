#pragma once

#include "../../utils/Definitions.h"

#include "Estimation.h"

namespace tomcat {
    namespace model {

        /**
         * Class responsible for computing estimates for a model in an offline
         * fashion. The test data received from the pipeline is used in batch to
         * compute the estimates.
         */
        class OfflineEstimation : public Estimation {
          public:
            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates an offline estimation process.
             *
             * @param estimator: type of estimation to be performed
             */
            OfflineEstimation(std::shared_ptr<Estimator> estimator);

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
            void estimate(DBNData test_data) override;

        };

    } // namespace model
} // namespace tomcat
