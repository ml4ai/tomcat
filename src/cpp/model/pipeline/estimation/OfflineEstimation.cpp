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
        // Static functions
        //----------------------------------------------------------------------
        void OfflineEstimation::run_estimation_thread(
            shared_ptr<Estimator> estimator, const EvidenceSet& test_data) {

            // Todo - estimators should only do one estimation per node instead
            //  of having a list of nodes to estimate. Because the data may be
            //  visible for some, but not to others. For instance, if the
            //  estimator is trying to estimate a value for a non-repeatable
            //  node, no data for that node should be presented, otherwise the
            //  estimate will be 100% certain.
            //  While this is not changed and the estimator can have multiple
            //  nodes to estimate, we rely on the fact that a estimator with a
            //  single node will be created in the pipeline when the example
            //  above is the case.
            EvidenceSet new_test_data = test_data;
            string node_label = estimator->get_estimates()[0].label;
            if (estimator->get_model()
                    ->get_nodes_by_label(node_label)
                    .size() == 1) {

                new_test_data.remove(node_label);
            }
            estimator->estimate(new_test_data);
        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void OfflineEstimation::estimate(EvidenceSet test_data) {
            // Execute each one of the estimators in a single thread.
            vector<thread> threads;
            threads.reserve(this->estimators.size());
            for (auto estimator : this->estimators) {
                thread estimation_thread(
                    run_estimation_thread, estimator, test_data);
                estimation_thread.join();
            }
        }

        void OfflineEstimation::get_info(nlohmann::json& json) const {
            EstimationProcess::get_info(json);
            json["process"] = "offline";
        }

    } // namespace model
} // namespace tomcat
