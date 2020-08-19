#pragma once

#include "Measure.h"

namespace tomcat {
    namespace model {

        /**
         * Class responsible for computing the F1 Score of the estimates
         * calculated for a given model by some estimator.
         */
        class F1Score : public Measure {
          public:
            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates an F1 Score measure.
             */
            F1Score(std::shared_ptr<Estimator> estimator);

            ~F1Score();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------
            F1Score(const F1Score& f1_score);

            F1Score& operator=(const F1Score& f1_score);

            F1Score(F1Score&&) = default;

            F1Score& operator=(F1Score&&) = default;

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------
            Eigen::MatrixXd evaluate(const EvidenceSet& test_data) const override;
        };

    } // namespace model
} // namespace tomcat
