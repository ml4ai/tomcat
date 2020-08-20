#include "F1Score.h"

namespace tomcat {
    namespace model {

        //----------------------------------------------------------------------
        // Definitions
        //----------------------------------------------------------------------

        // No definitions in this file

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
        Eigen::MatrixXd F1Score::evaluate(const EvidenceSet& test_data) const {
            // TODO - return the F1 Score of the estimates computed by the
            //  estimator
        }

    } // namespace model
} // namespace tomcat
