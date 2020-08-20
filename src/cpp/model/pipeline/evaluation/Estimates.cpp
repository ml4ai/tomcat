#include "Estimates.h"

namespace tomcat {
    namespace model {

        //----------------------------------------------------------------------
        // Definitions
        //----------------------------------------------------------------------

        // No definitions in this file

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        Estimates::Estimates(std::shared_ptr<Estimator> estimator) : Measure(estimator) {}

        Estimates::~Estimates() {}

        //----------------------------------------------------------------------
        // Copy & Move constructors/assignments
        //----------------------------------------------------------------------
        Estimates::Estimates(
            const Estimates& estimates) {
            this->copy_measure(estimates);
        }

        Estimates& Estimates::operator=(const Estimates& estimates) {
            this->copy_measure(estimates);
            return *this;
        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        Eigen::MatrixXd Estimates::evaluate(const EvidenceSet& test_data) const {
            // TODO - return estimates stored in the estimator.
        }

    } // namespace model
} // namespace tomcat
