#include "OfflineEstimation.h"

namespace tomcat {
    namespace model {

        //----------------------------------------------------------------------
        // Definitions
        //----------------------------------------------------------------------

        // No definitions in this file

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        OfflineEstimation::OfflineEstimation(
            std::shared_ptr<ModelEstimator> estimator)
            : ModelEstimation(estimator) {}

        OfflineEstimation::~OfflineEstimation() {}

        //----------------------------------------------------------------------
        // Copy & Move constructors/assignments
        //----------------------------------------------------------------------
        OfflineEstimation::OfflineEstimation(
            const OfflineEstimation& estimation) {
            this->copy_estimation(estimation);
        }

        OfflineEstimation&
        OfflineEstimation::operator=(const OfflineEstimation& estimation) {
            this->copy_estimation(estimation);
            return *this;
        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void OfflineEstimation::estimate(EvidenceSet test_data) {
            this->estimator->estimate(test_data);
        }

    } // namespace model
} // namespace tomcat
