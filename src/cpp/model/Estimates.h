#pragma once

#include "Measure.h"

namespace tomcat {
    namespace model {

        /**
         * Represents a measure given by estimates computed over a model by some
         * estimator.
         */
        class Estimates : public Measure {
          public:


            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates a measure based on estimates computed by some estimator.
             */
            Estimates(std::shared_ptr<Estimator> estimator);

            ~Estimates();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------
            Estimates(const Estimates& estimates);

            Estimates& operator=(const Estimates& estimates);

            Estimates(Estimates&&) = default;

            Estimates& operator=(Estimates&&) = default;

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------
            Eigen::MatrixXd evaluate(const EvidenceSet& test_data) const override;

        };

    } // namespace model
} // namespace tomcat
