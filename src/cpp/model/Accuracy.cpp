#include "Accuracy.h"

namespace tomcat {
    namespace model {

        //----------------------------------------------------------------------
        // Definitions
        //----------------------------------------------------------------------

        // No definitions in this file

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
        Eigen::MatrixXd Accuracy::evaluate(const EvidenceSet& test_data) const {
            // TODO - return the accuracy of the estimates computed by the
            //  estimator
        }

    } // namespace model
} // namespace tomcat
