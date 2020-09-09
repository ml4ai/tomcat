#include "Estimates.h"

namespace tomcat {
    namespace model {

        using namespace std;

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        Estimates::Estimates(shared_ptr<Estimator> estimator)
            : Measure(estimator) {}

        Estimates::~Estimates() {}

        //----------------------------------------------------------------------
        // Copy & Move constructors/assignments
        //----------------------------------------------------------------------
        Estimates::Estimates(const Estimates& estimates) {
            this->copy_measure(estimates);
        }

        Estimates& Estimates::operator=(const Estimates& estimates) {
            this->copy_measure(estimates);
            return *this;
        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        vector<NodeEvaluation>
        Estimates::evaluate(const EvidenceSet& test_data) const {
            vector<NodeEvaluation> evaluations;

            for (const auto& estimates : this->estimator->get_estimates()) {
                NodeEvaluation evaluation;
                evaluation.label = estimates.label;
                evaluation.assignment = estimates.assignment;

                // This measure doesn't compute anything over the estimates. It
                // just returns the raw estimates as evaluation.
                evaluation.evaluation = estimates.estimates;
                evaluations.push_back(evaluation);
            }

            return evaluations;
        }

        void Estimates::get_info(nlohmann::json& json) const {
            json["name"] = "raw estimates";
            this->estimator->get_info(json["estimator"]);
        }

    } // namespace model
} // namespace tomcat
