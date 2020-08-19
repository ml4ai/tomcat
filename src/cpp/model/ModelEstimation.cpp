#include "ModelEstimation.h"

namespace tomcat {
    namespace model {

        //----------------------------------------------------------------------
        // Definitions
        //----------------------------------------------------------------------

        // No definitions in this file

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        ModelEstimation::ModelEstimation() {}

        ModelEstimation::ModelEstimation(
            std::shared_ptr<ModelEstimator> estimator)
            : estimator(estimator) {}

        ModelEstimation::~ModelEstimation() {}

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void ModelEstimation::copy_estimation(const ModelEstimation& estimation) {
            this->estimator = estimation.estimator;
        }

        //----------------------------------------------------------------------
        // Getters & Setters
        //----------------------------------------------------------------------
        const std::shared_ptr<ModelEstimator>&
        ModelEstimation::get_estimator() const {
            return estimator;
        }

    } // namespace model
} // namespace tomcat
