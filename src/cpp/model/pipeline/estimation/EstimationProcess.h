#pragma once

#include <memory>

#include "pipeline/estimation/Estimator.h"
#include "utils/Definitions.h"

namespace tomcat {
    namespace model {

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

            /**
             * Aks estimators to save the estimates computed. In a cross
             * validation process, this method will be called multiple times. In
             * the end, we will have a list of estimates calculated in each one
             * of the cross validation steps.
             */
            void keep_estimates();

            /**
             * Clear cumulative estimates computed by the estimators.
             */
            void clear_estimates();

            //------------------------------------------------------------------
            // Virtual functions
            //------------------------------------------------------------------

            /**
             * Prepares the estimation process to start again.
             */
            virtual void prepare();

            /**
             * Writes information about the estimation in a json object.
             *
             * @param json: json object
             */
            virtual void get_info(nlohmann::json& json) const;

            //------------------------------------------------------------------
            // Pure virtual functions
            //------------------------------------------------------------------

            /**
             * Computes estimations for a model over a test data.
             *
             * @param test_data: Test data used to compute the estimations for
             * the model
             */
            virtual void estimate(const EvidenceSet& test_data) = 0;

            //------------------------------------------------------------------
            // Getters & Setters
            //------------------------------------------------------------------

            void set_display_estimates(bool display_estimates);

          protected:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Copies data members from another estimation process.
             */
            void copy_estimation(const EstimationProcess& estimation);

            /**
             * Computes estimations for a model over a test data using a given
             * estimator.
             *
             * @param estimator: estimator
             * @param test_data: test data used to compute the estimations for
             * the model
             */
            void estimate(std::shared_ptr<Estimator> estimator,
                          const EvidenceSet& test_data);

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------
            std::vector<std::shared_ptr<Estimator>> estimators;

            // Whether the estimates should be displayed in the evaluation file.
            bool display_estimates = false;

            // Estimates computed each time the estimation process is executed.
            std::vector<CumulativeNodeEstimates> cumulative_estimates;
        };

    } // namespace model
} // namespace tomcat
