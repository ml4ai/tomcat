#pragma once

#include "../../utils/Definitions.h"
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
             *
             * @param estimator: estimator used to compute the estimates
             * @param threshold: Probability threshold for predicting or
             * inferring the occurrence of an assignment as true
             */
            F1Score(std::shared_ptr<Estimator> estimator,
                    double threshold = 0.5);

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
            std::vector<NodeEvaluation>
            evaluate(const EvidenceSet& test_data) const override;

            void get_info(nlohmann::json& json) const override;
        };

    } // namespace model
} // namespace tomcat
