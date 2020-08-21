#pragma once

#include <memory>
#include <string>

#include "../../pgm/DBNData.h"
#include "../../utils/Definitions.h"
#include "../estimation/Estimator.h"
#include "../../utils/Tensor3.h"

namespace tomcat {
    namespace model {

        /**
         * Represents some measurement that can be performed over estimates.
         */
        class Measure {
          public:
            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates an empty measure.
             */
            Measure();

            /**
             * Creates an abstract measure.
             * @param estimator
             */
            Measure(std::shared_ptr<Estimator> estimator);

            virtual ~Measure();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------
            Measure(const Measure&) = delete;

            Measure& operator=(const Measure&) = delete;

            Measure(Measure&&) = default;

            Measure& operator=(Measure&&) = default;

            //------------------------------------------------------------------
            // Pure virtual functions
            //------------------------------------------------------------------

            /**
             * Returns the name of the measure.
             *
             * @return Measure's name.
             */
            virtual std::string get_name() const = 0;

            /**
             * Calculates the measure over a data set.
             *
             * @param test_data: data to calculate the measure over
             *
             * @return Computed values.
             */
            virtual DBNData evaluate(const DBNData& test_data) const = 0;

          protected:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Copies the data members from another measure.
             *
             * @param measure: measure to copy data members from
             */
            void copy_measure(const Measure& measure);

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------
            // The estimates computed and stored in the estimator will be used
            // to evaluate the measure.
            std::shared_ptr<Estimator> estimator;


        };

    } // namespace model
} // namespace tomcat
