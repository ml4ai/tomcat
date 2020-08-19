#include "MeasureAggregator.h"

namespace tomcat {
    namespace model {

        //----------------------------------------------------------------------
        // Definitions
        //----------------------------------------------------------------------

        // No definitions in this file

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        MeasureAggregator::MeasureAggregator(std::shared_ptr<std::ostream> output_stream)
            : output_stream(output_stream) {}

        MeasureAggregator::~MeasureAggregator() {}

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void MeasureAggregator::add_measure(std::shared_ptr<Measure> measure) {
            this->measures.push_back(measure);
        }

        void MeasureAggregator::aggregate(const EvidenceSet& test_data) {
            for (auto& measure : this->measures) {
                Eigen::MatrixXd result = measure.evaluate(test_data);

                // TODO - aggregate results
            }
        }

        void MeasureAggregator::dump() {
            // TODO - write aggregate results to the output stream.
        }

    } // namespace model
} // namespace tomcat
