#include "OfflineEstimation.h"

#include <thread>

namespace tomcat {
    namespace model {

        using namespace std;

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
        // Member functions
        //----------------------------------------------------------------------
        void OfflineEstimation::estimate(const EvidenceSet& test_data) {
            // Execute each one of the estimators in a single thread.
            for (auto estimator : this->estimators) {
                thread estimation_thread(
                    &OfflineEstimation::run_estimation_thread,
                    this,
                    estimator,
                    test_data);
                estimation_thread.join();
            }
        }

        void OfflineEstimation::run_estimation_thread(
            shared_ptr<Estimator> estimator, const EvidenceSet& test_data) {

            EstimationProcess::estimate(estimator, test_data);
        }

        void OfflineEstimation::get_info(nlohmann::json& json) const {
            EstimationProcess::get_info(json);
            json["process"] = "offline";
        }

    } // namespace model
} // namespace tomcat
