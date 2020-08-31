#pragma once

#include "../../utils/Definitions.h"

#include "../../pgm/inference/FactorGraph.h"
#include "Estimator.h"

namespace tomcat {
    namespace model {

        /**
         * Class description here
         */
        class SumProductEstimator : public Estimator {
          public:
            //------------------------------------------------------------------
            // Types, Enums & Constants
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates an instance of a sum-product estimator.
             *
             * @param model: DBN
             * @param inference_horizon: how many time steps in the future
             * estimations are going to be computed for
             */
            SumProductEstimator(std::shared_ptr<DynamicBayesNet> model,
                                int inference_horizon);

            ~SumProductEstimator();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------
            SumProductEstimator(const SumProductEstimator& estimator);

            SumProductEstimator&
            operator=(const SumProductEstimator& estimator);

            SumProductEstimator(SumProductEstimator&&) = default;

            SumProductEstimator& operator=(SumProductEstimator&&) = default;

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------
            void estimate(EvidenceSet new_data) override;

            void get_info(nlohmann::json& json) const override;

            std::string get_name() const override;

            FactorGraph factor_graph;

          protected:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------

          private:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------
            void compute_forward_messages(const FactorGraph& factor_graph,
                                          int time_step,
                                          const EvidenceSet& new_data);

            void compute_backward_messages(const FactorGraph& factor_graph,
                                           int time_step,
                                           const EvidenceSet& new_data);

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------

            // Next time step to compute messages to the nodes in the factor
            // graph. Nodes with time steps before next_time_step already have
            // their messages computed.
            int next_time_step = 0;


        };

    } // namespace model
} // namespace tomcat
