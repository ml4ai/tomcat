#pragma once

#include <memory>

#include "../../utils/Definitions.h"

#include "Estimator.h"

namespace tomcat {
    namespace model {

        //------------------------------------------------------------------
        // Forward declarations
        //------------------------------------------------------------------

        //------------------------------------------------------------------
        // Structs
        //------------------------------------------------------------------

        /**
         * Generic estimation process for a DBN model.
         */
        class Estimation {
          public:
            //------------------------------------------------------------------
            // Types, Enums & Constants
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates an empty estimation process for a model.
             */
            Estimation();

            /**
             * Creates an abstract estimation process for a model.
             *
             * @param estimator: type of estimation to be performed
             */
            Estimation(std::shared_ptr<Estimator> estimator);

            virtual ~Estimation();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------

            // Copy constructor and assignment should be deleted to avoid
            // implicit slicing and loss of polymorphic behaviour in the
            // subclasses. To deep copy, the clone method must be used.
            Estimation(const Estimation&) = delete;

            Estimation& operator=(const Estimation&) = delete;

            Estimation(Estimation&&) = default;

            Estimation& operator=(Estimation&&) = default;

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Assigns training data to the estimator. Some estimators (mainly
             * the baseline ones) might calculate estimates based on training
             * data rather than test data.
             *
             * @param training_data: training data
             */
            void set_training_data(const DBNData& training_data);

            //------------------------------------------------------------------
            // Virtual functions
            //------------------------------------------------------------------

            /**
             * Prepares the estimation process to start again.
             */
            virtual void reset();

            //------------------------------------------------------------------
            // Pure virtual functions
            //------------------------------------------------------------------

            /**
             * Computes estimations for a model over a test data.
             *
             * @param test_data: Test data used to compute the estimations for
             * the model
             */
            virtual void estimate(DBNData test_data) = 0;

            //------------------------------------------------------------------
            // Getters & Setters
            //------------------------------------------------------------------
            const std::shared_ptr<Estimator>& get_estimator() const;

            bool is_finished() const;

          protected:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Copies data members from another estimation process.
             */
            void copy_estimation(const Estimation& estimation);

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------
            std::shared_ptr<Estimator> estimator;

            bool finished = false;
        };

    } // namespace model
} // namespace tomcat
