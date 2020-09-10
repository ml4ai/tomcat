#pragma once

#include <iostream>
#include <memory>
#include <unordered_map>
#include <vector>

#include "model/pgm/EvidenceSet.h"
#include "model/utils/Definitions.h"
#include "model/pipeline/evaluation/Measure.h"

namespace tomcat {
    namespace model {

        //------------------------------------------------------------------
        // Structs
        //------------------------------------------------------------------

        /**
         * This struct stores an aggregated value computed over evaluations.
         */
        struct Aggregation {

            // This is defined as a vector because according to the aggregation
            // method chosen, no aggregation might be performed at all.
            std::vector<Eigen::MatrixXd> aggregated_values;

            // The error is a single matrix because, depending on the measure
            // chosen to evaluate nodes, the results can be matrices instead of a
            // single number. In that case, if the method chosen for aggregating
            // such evaluations is indeed a summarizer (it can just preserve
            // the evaluations with no aggregation at all), the resultant error
            // will be over a list of matrices, which will result in a single
            // matrix.
            Eigen::MatrixXd errors;
        };

        /**
         * This struct stores a node's label, assignment over which the
         * estimator performed its computations and the aggregated evaluations
         * calculated for that node.
         */
        struct NodeEvaluationAggregation {

            std::string label;

            Eigen::VectorXd assignment;

            Aggregation aggregated_evaluation;
        };

        /**
         * This class is responsible for computing and aggregating evaluations a
         * set of measures and nodes' estimates.
         */
        class EvaluationAggregator {
          public:
            //------------------------------------------------------------------
            // Types, Enums & Constants
            //------------------------------------------------------------------
            enum METHOD { no_aggregation, average };

            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates an instance of an evaluation aggregator.
             *
             * @param method: method used to aggregate the evaluations
             * dumped
             */
            EvaluationAggregator(METHOD method);

            ~EvaluationAggregator();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------
            EvaluationAggregator(const EvaluationAggregator&) = default;

            EvaluationAggregator&
            operator=(const EvaluationAggregator&) = default;

            EvaluationAggregator(EvaluationAggregator&&) = default;

            EvaluationAggregator& operator=(EvaluationAggregator&&) = default;

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Prepares the aggregator to start again.
             */
            void reset();

            /**
             * Adds a measure to the aggregator.
             *
             * @param measure: measure
             */
            void add_measure(std::shared_ptr<Measure> measure);

            /**
             * Evaluates the nodes and store the results for future aggregation.
             *
             * @param test_data: data to evaluate the measures
             */
            void evaluate(const EvidenceSet& test_data);

            /**
             * Aggregates evaluations previously computed.
             */
            void aggregate();

            /**
             * Writes information about the splitter in a json object.
             *
             * @param json: json object
             */
            void get_info(nlohmann::json& json) const;

          private:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Aggregates values for a list of evaluations according to the
             * aggregation method defined. The return is a vector because
             * according to the aggregation method chosen, no aggregation might
             * be performed at all.
             *
             * @param evaluations: Evaluations to aggregate
             *
             * @return Aggregated evaluations
             */
            Aggregation
            compute_aggregation(const std::vector<Eigen::MatrixXd>& evaluations) const;

            /**
             * Returns the name of the aggregation method used;
             *
             * @return Method's name
             */
            std::string get_method_name() const;

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------
            METHOD method = METHOD::no_aggregation;

            std::vector<std::shared_ptr<Measure>> measures;

            // This stores a list of evaluations per node for a certain measure.
            // A combination of measure and node can have multiple evaluations
            // because the pipeline may call the evaluate function multiple
            // times by using cross validation, for instance. The aggregator
            // will store all the evaluations in this data structure and it will
            // aggregate them only upon request.
            std::vector<std::vector<std::vector<NodeEvaluation>>>
                evaluations_per_measure;

            // This stores the aggregated values for the evaluations previously
            // computed and kept.
            std::vector<std::vector<NodeEvaluationAggregation>>
                aggregations_per_measure;
        };

    } // namespace model
} // namespace tomcat
