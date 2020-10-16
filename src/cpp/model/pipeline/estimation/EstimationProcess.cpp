#include "EstimationProcess.h"

#include "utils/EigenExtensions.h"

namespace tomcat {
    namespace model {

        using namespace std;

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        EstimationProcess::EstimationProcess() {}

        EstimationProcess::~EstimationProcess() {}

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void EstimationProcess::set_training_data(
            const tomcat::model::EvidenceSet& training_data) {
            for (auto& estimator : this->estimators) {
                estimator->set_training_data(training_data);
            }
        }

        void EstimationProcess::add_estimator(shared_ptr<Estimator> estimator) {
            this->estimators.push_back(estimator);
        }

        void EstimationProcess::keep_estimates() {
            for (auto& estimator : this->estimators) {
                estimator->keep_estimates();
            }
        }

        void EstimationProcess::clear_estimates() {
            for (auto& estimator : this->estimators) {
                estimator->clear_estimates();
            }
        }

        void EstimationProcess::prepare() {
            for (auto& estimator : this->estimators) {
                estimator->prepare();
            }
        }

        void EstimationProcess::copy_estimation(
            const EstimationProcess& estimation) {
            this->estimators = estimation.estimators;
        }

        void EstimationProcess::estimate(shared_ptr<Estimator> estimator, const EvidenceSet& test_data) {
            EvidenceSet new_test_data = test_data;
            string node_label = estimator->get_estimates().label;
            // Remove data from nodes that only have one instance in the
            // unrolled DBN, because the true data don't change over time and
            // providing any data for these nodes, will make the estimator
            // "cheat" as it will have access to the true value. For nodes that
            // have multiple copies this is not a problem as the estimator only
            // look at past data and, the value of that node can change over
            // time.
            if (estimator->get_model()->get_nodes_by_label(node_label).size() ==
                1) {

                new_test_data.remove(node_label);
            }
            estimator->estimate(new_test_data);
        }

        void EstimationProcess::get_info(nlohmann::json& json) const {
            if (this->display_estimates) {
                json["estimators"] = nlohmann::json::array();
                for (const auto& estimator : this->estimators) {
                    nlohmann::json json_estimator;
                    estimator->get_info(json_estimator);

                    CumulativeNodeEstimates cumulative_estimates =
                        estimator->get_cumulative_estimates();

                    json_estimator["node_label"] = cumulative_estimates.label;
                    json_estimator["node_assignment"] =
                        to_string(cumulative_estimates.assignment);
                    json_estimator["executions"] = nlohmann::json::array();

                    for (const auto& estimates_matrix_per_execution :
                         cumulative_estimates.estimates) {

                        nlohmann::json json_execution;
                        json_execution["estimates"] = nlohmann::json::array();

                        for (const auto& estimates_matrix :
                             estimates_matrix_per_execution) {
                            json_execution["estimates"].push_back(
                                to_string(estimates_matrix));
                        }

                        json_estimator["executions"].push_back(json_execution);
                    }

                    json["estimators"].push_back(json_estimator);
                }
            }
        }

        //----------------------------------------------------------------------
        // Getters & Setters
        //----------------------------------------------------------------------

        void EstimationProcess::set_display_estimates(bool display_estimates) {
            EstimationProcess::display_estimates = display_estimates;
        }

    } // namespace model
} // namespace tomcat
