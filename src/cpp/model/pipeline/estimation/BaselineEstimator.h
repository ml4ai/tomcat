#pragma once

#include <unordered_set>

#include "../../utils/Definitions.h"

#include "Estimator.h"

namespace tomcat {
    namespace model {

        /**
         * This estimator is based on the relative frequencies of the
         * observations over the training data for each time step on an unrolled
         * DBN. For instance, the probability of observing a value x in time
         * step t for a given node will be proportional to the number of times
         * the value x is observed in the training data for that node at time t.
         */
        class BaselineEstimator : public Estimator {
          public:
            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates an instance of a baseline estimator.
             *
             * @param model: DBN
             * @param inference_horizon: how many time steps in the future
             * estimations are going to be computed for
             */
            BaselineEstimator(std::shared_ptr<DynamicBayesNet> model,
                              int inference_horizon);

            ~BaselineEstimator();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------
            BaselineEstimator(const BaselineEstimator& estimator);

            BaselineEstimator& operator=(const BaselineEstimator& estimator);

            BaselineEstimator(BaselineEstimator&&) = default;

            BaselineEstimator& operator=(BaselineEstimator&&) = default;

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------
            void estimate(DBNData new_data) override;

            DBNData
            get_last_estimates(int initial_time_step) const override;

            void add_node(const std::string node_label) override;

            void set_training_data(const DBNData& training_data) override;

          private:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------

            // Every time a new training set is assigned to the estimator, its
            // node's estimates will need to be recalculated. The same if a new
            // node is added to the estimator. This variable keeps track of the
            // nodes that need to have their estimates recalculated.
            std::unordered_set<std::string> nodes_to_reestimate;
        };

    } // namespace model
} // namespace tomcat
