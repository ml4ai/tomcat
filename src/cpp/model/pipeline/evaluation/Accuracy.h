#pragma once

#include "pipeline/evaluation/Measure.h"
#include "utils/Definitions.h"

namespace tomcat {
    namespace model {

        /**
         * Class responsible for computing the accuracy of the estimates
         * calculated for a given model by some estimator.
         */
        class Accuracy : public Measure {
          public:
            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates an accuracy measure.
             *
             * @param estimator: estimator used to compute the estimates
             * @param threshold: Probability threshold for predicting or
             * inferring the occurrence of an assignment as true
             */
            Accuracy(std::shared_ptr<Estimator> estimator,
                     double threshold = 0.5,
                     bool use_last_estimate = false);

            ~Accuracy();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------
            Accuracy(const Accuracy& accuracy);

            Accuracy& operator=(const Accuracy& accuracy);

            Accuracy(Accuracy&&) = default;

            Accuracy& operator=(Accuracy&&) = default;

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------
            NodeEvaluation
            evaluate(const EvidenceSet& test_data) const override;

            void get_info(nlohmann::json& json) const override;
        };

    } // namespace model
} // namespace tomcat
