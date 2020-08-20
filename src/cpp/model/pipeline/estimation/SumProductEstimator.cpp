#include "SumProductEstimator.h"

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
        SumProductEstimator::SumProductEstimator(
            std::shared_ptr<DynamicBayesNet> model, int inference_horizon)
            : Estimator(model, inference_horizon) {}

        SumProductEstimator::~SumProductEstimator() {}

        //----------------------------------------------------------------------
        // Copy & Move constructors/assignments
        //----------------------------------------------------------------------
        SumProductEstimator::SumProductEstimator(
            const SumProductEstimator& estimator) {
            this->copy_estimator(estimator);
        }

        SumProductEstimator&
        SumProductEstimator::operator=(const SumProductEstimator& estimator) {
            this->copy_estimator(estimator);
            return *this;
        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void SumProductEstimator::estimate(EvidenceSet new_data) {
            // TODO
            std::cout << "Sum-Product Estimation" << std::endl;
        }

        Eigen::MatrixXd
        SumProductEstimator::get_last_estimates(const std::string& node_label,
                                                int initial_time_step) const {
            // TODO
            return Eigen::MatrixXd(0,0);
        }

    } // namespace model
} // namespace tomcat
