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
        std::string F1Score::get_name() const {
            return "F1-Score";
        }

        DBNData F1Score::evaluate(const DBNData& test_data) const {
            // TODO - return the F1 Score of the estimates computed by the
            //  estimator

            return DBNData();
        }

    } // namespace model
} // namespace tomcat
