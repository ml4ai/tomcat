#pragma once

#include "../../utils/Definitions.h"
#include "Measure.h"

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
             */
            Accuracy(std::shared_ptr<Estimator> estimator);

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
            std::vector<NodeEvaluation> evaluate(const EvidenceSet& test_data) const override;

            void get_info(nlohmann::json& json) const override;
        };

    } // namespace model
} // namespace tomcat
