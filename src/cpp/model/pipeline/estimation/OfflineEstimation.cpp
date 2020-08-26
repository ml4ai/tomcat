#include "OfflineEstimation.h"

#include <thread>

namespace tomcat {
    namespace model {

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        OfflineEstimation::OfflineEstimation() {}

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
        // Static functions
        //----------------------------------------------------------------------
        void OfflineEstimation::run_estimation_thread(
            std::shared_ptr<Estimator> estimator,
            const EvidenceSet& test_data) {

            estimator->estimate(test_data);
        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void OfflineEstimation::estimate(EvidenceSet test_data) {
            // Execute each one of the estimators in a single thread.
            std::vector<std::thread> threads;
            threads.reserve(this->estimators.size());
            for (auto estimator : this->estimators) {
                std::thread estimation_thread(
                    run_estimation_thread, estimator, test_data);
                estimation_thread.join();
            }
        }

    } // namespace model
} // namespace tomcat
