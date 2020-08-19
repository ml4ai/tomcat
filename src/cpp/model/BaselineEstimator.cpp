#include "BaselineEstimator.h"

#include <iostream>

namespace tomcat {
    namespace model {

        //----------------------------------------------------------------------
        // Definitions
        //----------------------------------------------------------------------

        // No definitions in this file

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        BaselineEstimator::BaselineEstimator(
            std::shared_ptr<DynamicBayesNet> model, int inference_horizon)
            : ModelEstimator(model, inference_horizon) {}

        BaselineEstimator::~BaselineEstimator() {}

        //----------------------------------------------------------------------
        // Copy & Move constructors/assignments
        //----------------------------------------------------------------------
        BaselineEstimator::BaselineEstimator(
            const BaselineEstimator& estimator) {
            this->copy_estimator(estimator);
        }

        BaselineEstimator&
        BaselineEstimator::operator=(const BaselineEstimator& estimator) {
            this->copy_estimator(estimator);
            return *this;
        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void BaselineEstimator::estimate(EvidenceSet new_data) {
            // TODO
            std::cout << "Baseline Estimation" << std::endl;
        }

        Eigen::MatrixXd
        BaselineEstimator::get_last_estimates(const std::string& node_label,
                             int initial_time_step) const {
            // TODO
            return Eigen::MatrixXd(0,0);
        }

    } // namespace model
} // namespace tomcat
