#pragma once

#include <iostream>
#include <memory>
#include <vector>

#include "Measure.h"

namespace tomcat {
    namespace model {

        /**
         * This class is responsible for aggregating evaluations of a set of
         * measures and dump such aggregated value(s) to an output stream.
         */
        class MeasureAggregator {
          public:
            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates an instance of a measure aggregator.
             *
             * @param output_stream: where the results will be written when
             * dumped
             */
            MeasureAggregator(std::shared_ptr<std::ostream> output_stream);

            ~MeasureAggregator();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------
            MeasureAggregator(const MeasureAggregator&) = default;

            MeasureAggregator& operator=(const MeasureAggregator&) = default;

            MeasureAggregator(MeasureAggregator&&) = default;

            MeasureAggregator& operator=(MeasureAggregator&&) = default;

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Adds a measure to the aggregator.
             *
             * @param measure: measure
             */
            void add_measure(std::shared_ptr<Measure> measure);

            /**
             * Aggregates results computed by the measures added to the
             * aggregator.
             *
             * @param test_data: data to evaluate the measures
             */
            void aggregate(const EvidenceSet& test_data);

            /**
             * Writes the aggregated results to an output_stream.
             */
            void dump();

          private:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------
            std::shared_ptr<std::ostream> output_stream;

            std::vector<Measure> measures;
        };

    } // namespace model
} // namespace tomcat
