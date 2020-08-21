#pragma once

#include <iostream>
#include <memory>
#include <unordered_map>
#include <vector>

#include "../../utils/Definitions.h"
#include "../../pgm/DBNData.h"
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
            MeasureAggregator(std::ostream& output_stream);

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
             * Aggregates results computed by the measures added to the
             * aggregator.
             *
             * @param test_data: data to evaluate the measures
             */
            void aggregate(const DBNData& test_data);

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
            std::reference_wrapper<std::ostream> output_stream;

            std::vector<std::shared_ptr<Measure>> measures;

            std::unordered_map<std::string, std::vector<DBNData>>
                measures_to_results;
        };

    } // namespace model
} // namespace tomcat
