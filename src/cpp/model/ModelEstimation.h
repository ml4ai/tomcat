#pragma once

#include <memory>

#include "ModelEstimator.h"

namespace tomcat {
    namespace model {

        //------------------------------------------------------------------
        // Forward declarations
        //------------------------------------------------------------------

        //------------------------------------------------------------------
        // Structs
        //------------------------------------------------------------------

        /**
         * Class description here
         */
        class ModelEstimation {
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
            ModelEstimation();

            /**
             * Creates an abstract estimation process for a model.
             *
             * @param estimator: type of estimation to be performed
             */
            ModelEstimation(std::shared_ptr<ModelEstimator> estimator);

            virtual ~ModelEstimation();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------

            // Copy constructor and assignment should be deleted to avoid
            // implicit slicing and loss of polymorphic behaviour in the
            // subclasses. To deep copy, the clone method must be used.
            ModelEstimation(const ModelEstimation&) = delete;

            ModelEstimation& operator=(const ModelEstimation&) = delete;

            ModelEstimation(ModelEstimation&&) = default;

            ModelEstimation& operator=(ModelEstimation&&) = default;

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

            //------------------------------------------------------------------
            // Getters & Setters
            //------------------------------------------------------------------
            const std::shared_ptr<ModelEstimator>& get_estimator() const;

          protected:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Copies data members from another estimation process.
             */
            void copy_estimation(const ModelEstimation& estimation);

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------
            std::shared_ptr<ModelEstimator> estimator;

        };

    } // namespace model
} // namespace tomcat
