#pragma once

#include <memory>
#include <string>

#include <nlohmann/json.hpp>

#include "../../pgm/EvidenceSet.h"
#include "../../utils/Definitions.h"
#include "../../utils/Tensor3.h"
#include "../estimation/Estimator.h"

namespace tomcat {
    namespace model {

        //------------------------------------------------------------------
        // Structs
        //------------------------------------------------------------------

        /**
         * This struct stores the counts of the 4 components of a confusion
         * matrix.
         */
        struct ConfusionMatrix {

            int true_positives = 0;

            int false_positives = 0;

            int true_negatives = 0;

            int false_negatives = 0;

            int get_total() const {
                return true_positives + true_negatives + false_positives +
                       false_negatives;
            }
        };

        /**
         * This struct stores a node's label, assignment over which the
         * estimator performed its computations and the evaluations calculated
         * for that node.
         */
        struct NodeEvaluation {

            std::string label;

            Eigen::VectorXd assignment;

            Eigen::MatrixXd evaluation;
        };

        /**
         * Represents some measurement that can be performed over estimates.
         *
         */
        class Measure {
          public:
            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates an empty measure.
             *
             * @param threshold: Probability threshold for predicting or
             * inferring the occurrence of an assignment as true
             */
            Measure();

            /**
             * Creates an abstract measure.
             *
             * @param estimator: estimator used to compute the estimates
             * @param threshold: Probability threshold for predicting or
             * inferring the occurrence of an assignment as true
             */
            Measure(std::shared_ptr<Estimator> estimator, double threshold = 0.5);

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
             * Calculates the measure over a data set.
             *
             * @param test_data: data to calculate the measure over
             *
             * @return Computed values for all of the nodes processed by the
             * estimator.
             */
            virtual std::vector<NodeEvaluation>
            evaluate(const EvidenceSet& test_data) const = 0;

            /**
             * Writes information about the splitter in a json object.
             *
             * @param json: json object
             */
            virtual void get_info(nlohmann::json& json) const = 0;

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

            /**
             * Computes the confusion matrix between real values and estimates
             * previously computed for a model.
             *
             * @param estimates: estimates previously computed for the model
             * @param test_data: data with real values to compare the estimates
             * against
             *
             * @return Confusion matrix.
             */
            ConfusionMatrix
            get_confusion_matrix(const NodeEstimates& estimates,
                                 const EvidenceSet& test_data) const;

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------
            // The estimates computed and stored in the estimator will be used
            // to evaluate the measure.
            std::shared_ptr<Estimator> estimator;

            // Probability threshold for predicting or inferring the occurrence
            // of an assignment as true
            double threshold = 0.5;
        };

    } // namespace model
} // namespace tomcat
