#include "Accuracy.h"

namespace tomcat {
    namespace model {

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        Accuracy::Accuracy(std::shared_ptr<Estimator> estimator)
            : Measure(estimator) {}

        Accuracy::~Accuracy() {}

        //----------------------------------------------------------------------
        // Copy & Move constructors/assignments
        //----------------------------------------------------------------------
        Accuracy::Accuracy(const Accuracy& accuracy) {
            this->copy_measure(accuracy);
        }

        Accuracy& Accuracy::operator=(const Accuracy& accuracy) {
            this->copy_measure(accuracy);
            return *this;
        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        std::string Accuracy::get_name() const {
            return "Accuracy";
        }

        DBNData Accuracy::evaluate(const DBNData& test_data) const {
            // TODO - return the accuracy of the estimates computed by the
            //  estimator
            return DBNData();
        }

    } // namespace model
} // namespace tomcat
