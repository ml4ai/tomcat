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

        void
        EstimationProcess::add_estimator(shared_ptr<Estimator> estimator) {
            this->estimators.push_back(estimator);
        }

        void EstimationProcess::reset() {
            for(auto& estimator :this->estimators){
                estimator->prepare();
            }
        }

        void EstimationProcess::copy_estimation(
            const EstimationProcess& estimation) {
            this->estimators = estimation.estimators;
        }

        void EstimationProcess::get_info(nlohmann::json& json) const {
            if (this->display_estimates) {
                json["estimators"] = nlohmann::json::array();
                for (const auto& estimator : this->estimators) {
                    nlohmann::json json_estimator;
                    estimator->get_info(json_estimator);
                    json_estimator["estimates"] = nlohmann::json::array();

                    for(const auto& estimates_per_node : estimator->get_estimates()) {
                        nlohmann::json json_estimate;
                        json_estimate["values"] = nlohmann::json::array();
                        for(const auto estimates_matrix : estimates_per_node.estimates){
                            json_estimate["values"].push_back(to_string(estimates_matrix));
                        }
                        json_estimate["node_label"] = estimates_per_node.label;
                        json_estimate["node_assignment"] = to_string(estimates_per_node.assignment);
                        json_estimator["estimates"].push_back(json_estimate);
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
