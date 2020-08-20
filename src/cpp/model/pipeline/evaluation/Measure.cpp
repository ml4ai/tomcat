#include "Measure.h"

namespace tomcat {
    namespace model {

        //----------------------------------------------------------------------
        // Definitions
        //----------------------------------------------------------------------

        // No definitions in this file

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        Measure::Measure() {}

        Measure::Measure(std::shared_ptr<Estimator> estimator)
            : estimator(estimator) {}

        Measure::~Measure() {}

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void Measure::copy_measure(const Measure& measure) {
            this->estimator = measure.estimator;
        }

    } // namespace model
} // namespace tomcat
