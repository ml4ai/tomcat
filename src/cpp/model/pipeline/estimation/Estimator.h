#pragma once

#include <memory>
#include <set>
#include <string>
#include <unordered_set>
#include <vector>

#include <eigen3/Eigen/Dense>
#include <nlohmann/json.hpp>

#include "../../pgm/DynamicBayesNet.h"
#include "../../pgm/EvidenceSet.h"
#include "../../utils/Definitions.h"
#include "../../utils/Tensor3.h"

namespace tomcat {
    namespace model {

        //------------------------------------------------------------------
        // Structs
        //------------------------------------------------------------------

        /**
         * This struct stores a node's label, assignment over which the
         * estimator must perform its computations and the estimates calculated
         * for that node.
         */
        struct NodeEstimates {

            std::string label;

            Eigen::VectorXd assignment;

            // Probabilities or densities calculated for n data points over
            // several time steps.
            Eigen::MatrixXd estimates;
        };

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
            Estimator(std::shared_ptr<DynamicBayesNet> model, int inference_horizon);

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
            // Member functions
            //------------------------------------------------------------------

            /**
             * Adds a new node to have its assignment estimated over time.
             *
             * @param node: node's label and assignment
             */
            void add_node(const std::string& node_label,
                          const Eigen::VectorXd& assignment);

            /**
             * Returns estimates at a given time step, for all nodes processed
             * by the estimator.
             *
             * @param time_step: First time step to get the estimates
             * from
             *
             * @return Series of estimates for the nodes in the estimator.
             */
            std::vector<NodeEstimates> get_estimates_at(int time_step) const;

            /**
             * Returns all the estimates computed so far, for all nodes
             * processed by the estimator.
             *
             * @return Series of estimates for the nodes in the estimator.
             */
            std::vector<NodeEstimates> get_estimates() const;

            //------------------------------------------------------------------
            // Virtual functions
            //------------------------------------------------------------------

            /**
            * Initializations before the computation of estimates.
            *
            */
            virtual void prepare();

            //------------------------------------------------------------------
            // Pure virtual functions
            //------------------------------------------------------------------

            /**
             * Computes new estimates for the new data. New data consists of
             * observed values for time steps after the last processed one.
             *
             * @param new_data: observed values for time steps not already
             * seen by the method
             */
            virtual void estimate(EvidenceSet new_data) = 0;

            /**
             * Writes information about the splitter in a json object.
             *
             * @param json: json object
             */
            virtual void get_info(nlohmann::json& json) const = 0;

            /**
             * Returns the name of the estimator.
             *
             * @return Estimator's name.
             */
            virtual std::string get_name() const = 0;

            //------------------------------------------------------------------
            // Getters & Setters
            //------------------------------------------------------------------
            int get_inference_horizon() const;

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

            // List of nodes to estimate, their assignments and estimates
            std::vector<NodeEstimates> nodes_estimates;

            // An inference horizon determines if the task is a prediction (> 0)
            // or an inference (= 0). If it's a prediction, the horizon
            // determines up to how much further in the future predictions are
            // made.
            int inference_horizon;
        };

    } // namespace model
} // namespace tomcat
