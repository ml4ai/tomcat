#pragma once

#include "Estimator.h"

namespace tomcat {
    namespace model {

        /**
         * Class description here
         */
        class BaselineEstimator : public Estimator {
          public:
            //------------------------------------------------------------------
            // Types, Enums & Constants
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates an instance of a baseline estimator.
             *
             * @param model: DBN
             * @param inference_horizon: how many time steps in the future
             * estimations are going to be computed for
             */
            BaselineEstimator(std::shared_ptr<DynamicBayesNet> model,
                              int inference_horizon);

            ~BaselineEstimator();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------
            BaselineEstimator(const BaselineEstimator& estimator);

            BaselineEstimator& operator=(const BaselineEstimator& estimator);

            BaselineEstimator(BaselineEstimator&&) = default;

            BaselineEstimator& operator=(BaselineEstimator&&) = default;

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
