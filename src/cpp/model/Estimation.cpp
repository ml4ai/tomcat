#include "Estimation.h"

namespace tomcat {
    namespace model {

        //----------------------------------------------------------------------
        // Definitions
        //----------------------------------------------------------------------

        // No definitions in this file

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        Estimation::Estimation() {}

        Estimation::Estimation(
            std::shared_ptr<Estimator> estimator)
            : estimator(estimator) {}

        Estimation::~Estimation() {}

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void Estimation::copy_estimation(const Estimation& estimation) {
            this->estimator = estimation.estimator;
        }

        //----------------------------------------------------------------------
        // Getters & Setters
        //----------------------------------------------------------------------
        const std::shared_ptr<Estimator>&
        Estimation::get_estimator() const {
            return estimator;
        }

    } // namespace model
} // namespace tomcat
