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
        class EstimationProcess {
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
            EstimationProcess();

            virtual ~EstimationProcess();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------

            // Copy constructor and assignment should be deleted to avoid
            // implicit slicing and loss of polymorphic behaviour in the
            // subclasses. To deep copy, the clone method must be used.
            EstimationProcess(const EstimationProcess&) = delete;

            EstimationProcess& operator=(const EstimationProcess&) = delete;

            EstimationProcess(EstimationProcess&&) = default;

            EstimationProcess& operator=(EstimationProcess&&) = default;

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
            void set_training_data(const EvidenceSet& training_data);

            /**
             * Adds a new estimator to the estimation process.
             *
             * @param estimator: Estimator
             */
            void add_estimator(std::shared_ptr<Estimator> estimator);

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
            virtual void estimate(EvidenceSet test_data) = 0;

          protected:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Copies data members from another estimation process.
             */
            void copy_estimation(const EstimationProcess& estimation);

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------
            std::vector<std::shared_ptr<Estimator>> estimators;
        };

    } // namespace model
} // namespace tomcat
