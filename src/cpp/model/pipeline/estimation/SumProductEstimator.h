#pragma once

#include "../../utils/Definitions.h"

#include "../../pgm/inference/FactorGraph.h"
#include "Estimator.h"

namespace tomcat {
    namespace model {

        /**
         * This estimator computes the estimates of a node by using
         * message-passing sum-product algorithm on top of a factor graph
         * originated from an unrolled DBN.
         */
        class SumProductEstimator : public Estimator {
          public:
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
            void prepare() override;

            void estimate(EvidenceSet new_data) override;

            void get_info(nlohmann::json& json) const override;

            std::string get_name() const override;

            FactorGraph factor_graph;

          private:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Computes messages from parents to child nodes in a given time
             * step.
             *
             * @param factor_graph: factor graph used to compute messages
             * @param time_step: time step to process
             * @param new_data: observed values for time steps not already
             * processed by the method
             */
            void compute_forward_messages(const FactorGraph& factor_graph,
                                          int time_step,
                                          const EvidenceSet& new_data);

            /**
             * Computes messages from children to parent nodes in a given time
             * step.
             *
             * @param factor_graph: factor graph used to compute messages
             * @param time_step: time step to process
             * @param new_data: observed values for time steps not already
             * processed by the method
             */
            void compute_backward_messages(const FactorGraph& factor_graph,
                                           int time_step,
                                           const EvidenceSet& new_data);

            /**
             * Computes the a node's marginal for a given assignment in the
             * future, according to the the horizons provided to the estimator;
             *
             * @param node_label: node's label
             * @param time_step: current estimated time step. The predictions
             * are made beyond this time step up to the limit of the maximum
             * horizon provided to the estimator
             * @param num_data_points: number of data points in the data set
             * used for estimation
             *
             * @return Estimates for each data point in a single time step.
             */
            Eigen::VectorXd get_predictions_for(const std::string& node_label,
                                                int time_step,
                                                int assignment,
                                                int num_data_points);

            /**
             * Appends the estimates matrix with a new column.
             *
             * @param estimates: estimates for a given node
             * @param new_column: new column with new estimates
             */
            void add_column_to_estimates(NodeEstimates& estimates,
                                         const Eigen::VectorXd new_column);

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
