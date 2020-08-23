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
         */
        class Measure {
          public:
            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates an empty measure.
             */
            Measure();

            /**
             * Creates an abstract measure.
             * @param estimator
             */
            Measure(std::shared_ptr<Estimator> estimator);

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

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------
            // The estimates computed and stored in the estimator will be used
            // to evaluate the measure.
            std::shared_ptr<Estimator> estimator;
        };

    } // namespace model
} // namespace tomcat
