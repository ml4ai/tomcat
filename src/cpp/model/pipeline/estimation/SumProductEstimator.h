#pragma once

#include "Estimator.h"

namespace tomcat {
    namespace model {

        /**
         * Class description here
         */
        class SumProductEstimator : public Estimator {
          public:
            //------------------------------------------------------------------
            // Types, Enums & Constants
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates an instance of a sum-product estimator.
             *
             * @param model: DBN
             * @param inference_horizon: how many time steps in the future
             * estimations are going to be computed for
             */
            SumProductEstimator(std::shared_ptr<DynamicBayesNet> model,
                              int inference_horizon);

            ~SumProductEstimator();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------
            SumProductEstimator(const SumProductEstimator& estimator);

            SumProductEstimator& operator=(const SumProductEstimator& estimator);

            SumProductEstimator(SumProductEstimator&&) = default;

            SumProductEstimator& operator=(SumProductEstimator&&) = default;

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------
            void estimate(EvidenceSet new_data) override;

            Eigen::MatrixXd get_last_estimates(const std::string& node_label,
                                 int initial_time_step) const override;

          protected:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------

          private:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------
        };

    } // namespace model
} // namespace tomcat