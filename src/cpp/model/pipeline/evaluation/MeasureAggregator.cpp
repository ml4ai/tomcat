#include "MeasureAggregator.h"

namespace tomcat {
    namespace model {

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        MeasureAggregator::MeasureAggregator(
            std::ostream& output_stream)
            : output_stream(output_stream) {}

        MeasureAggregator::~MeasureAggregator() {}

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void MeasureAggregator::reset() { this->measures_to_results.clear(); }

        void MeasureAggregator::add_measure(std::shared_ptr<Measure> measure) {
            // TODO check for duplicity. Maybe change to a set.
            this->measures.push_back(measure);
            this->measures_to_results[measure->get_name()] = {};
        }

        void MeasureAggregator::aggregate(const DBNData& test_data) {
            for (auto& measure : this->measures) {
                DBNData result = measure->evaluate(test_data);
                this->measures_to_results[measure->get_name()].push_back(
                    result);
            }
        }

        void MeasureAggregator::dump() {
            // TODO - write aggregate results to the output stream. Just write
            //  the values to the stream for a while.
            //  For the final implementation, the aggregator will have a list of
            //  aggregation functions that might be applied over the results.

            for(const auto&[measure, results] : this->measures_to_results) {
                for(const auto& data : results) {
                    for(const auto& node_label : data.get_node_labels()) {
                        this->output_stream << data[node_label];
                    }
                }
            }
        }

    } // namespace model
} // namespace tomcat
