#include "F1Score.h"

namespace tomcat {
    namespace model {

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        F1Score::F1Score(std::shared_ptr<Estimator> estimator)
            : Measure(estimator) {}

        F1Score::~F1Score() {}

        //----------------------------------------------------------------------
        // Copy & Move constructors/assignments
        //----------------------------------------------------------------------
        F1Score::F1Score(const F1Score& f1_score) {
            this->copy_measure(f1_score);
        }

        F1Score& F1Score::operator=(const F1Score& f1_score) {
            this->copy_measure(f1_score);
            return *this;
        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        std::vector<NodeEvaluation> F1Score::evaluate(const EvidenceSet& test_data) const {
            // TODO - return the F1 Score of the estimates computed by the
            //  estimator

            return {};
        }

        void F1Score::get_info(nlohmann::json& json) const {
            json["name"] = "f1 score";
            this->estimator->get_info(json["estimator"]);
        }

    } // namespace model
} // namespace tomcat
