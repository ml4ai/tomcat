#pragma once

#include <memory>
#include <string>
#include <unordered_set>
#include <vector>

#include <eigen3/Eigen/Dense>

#include "../../pgm/DynamicBayesNet.h"
#include "../../utils/EvidenceSet.h"

namespace tomcat {
    namespace model {

        /**
         * Represents a generic estimator for a DBN model.
         */
        class Estimator {
          public:
            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates an empty estimator.
             */
            Estimator();

            /**
             * Creates an abstract estimator.
             *
             * @param model: DBN
             * @param inference_horizon: how many time steps in the future
             * estimations are going to be computed for
             */
            Estimator(std::shared_ptr<DynamicBayesNet> model,
                           int inference_horizon);

            virtual ~Estimator();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------

            // Copy constructor and assignment should be deleted to avoid
            // implicit slicing and loss of polymorphic behaviour in the
            // subclasses. To deep copy, the clone method must be used.
            Estimator(const Estimator&) = delete;

            Estimator& operator=(const Estimator&) = delete;

            Estimator(Estimator&&) = default;

            Estimator& operator=(Estimator&&) = default;

            //------------------------------------------------------------------
            // Pure virtual functions
            //------------------------------------------------------------------

            /**
             * Computes new estimates for the new data. New data consists of
             * observed values for time steps after the last processed one.
             *
             * @param new_data: Observed values for time steps not already
             * seen by the method
             */
            virtual void estimate(EvidenceSet new_data) = 0;

            /**
             * Returns estimates from a given initial time step until the last
             * time step processed by the method, for a given node.
             *
             * @param node_label: node's label
             * @param initial_time_step: First time step to get the estimates
             * from
             *
             * @return Series of estimates for a given node.
             */
            virtual Eigen::MatrixXd
            get_last_estimates(const std::string& node_label,
                                 int initial_time_step) const = 0;

            /**
             * Adds a new node to have its assignment estimated over time.
             *
             * @param node_label: node's label
             */
            void add_node(const std::string node_label);

            //------------------------------------------------------------------
            // Getters & Setters
            //------------------------------------------------------------------
            void set_training_data(const EvidenceSet& training_data);

          protected:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Copies data members from another estimator.
             */
            void copy_estimator(const Estimator& estimator);

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------
            std::shared_ptr<DynamicBayesNet> model;

            // Data used to train the model. Baseline methods can use the
            // information in the training set to compute their estimations
            // instead of test data.
            EvidenceSet training_data;

            // Observed data to perform estimations. More data points can be
            // appended as estimations are made. Each derived class must store
            // computations to avoid recalculations as new data is available.
            EvidenceSet test_data;

            // Labels of the nodes which the estimation will be performed for.
            std::unordered_set<std::string> node_labels;

            // Last time step used in the estimation.
            int last_processed_time_step;

            // Determines if the task is a prediction (> 0) or an inference (=
            // 0). If it's a prediction, how much further in the future
            // predictions are made.
            int inference_horizon;
        };

    } // namespace model
} // namespace tomcat
