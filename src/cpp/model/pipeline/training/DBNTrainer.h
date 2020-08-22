#pragma once

#include <nlohmann/json.hpp>

#include "../../pgm/EvidenceSet.h"
#include "../../utils/Definitions.h"

namespace tomcat {
    namespace model {

        /**
         * Class responsible for estimating a model's parameters.
         */
        class DBNTrainer {
          public:
            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates an abstract trainer.
             */
            DBNTrainer();

            virtual ~DBNTrainer();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------

            // Copy constructor and assignment should be deleted to avoid
            // implicit slicing and loss of polymorphic behaviour in the
            // subclasses.
            DBNTrainer(const DBNTrainer&) = delete;

            DBNTrainer& operator=(const DBNTrainer&) = delete;

            DBNTrainer(DBNTrainer&&) = default;

            DBNTrainer& operator=(DBNTrainer&&) = default;

            //------------------------------------------------------------------
            // Pure virtual functions
            //------------------------------------------------------------------

            /**
             * Prepares the trainer to a series of calls to the function fit by
             * performing necessary cleanups.
             */
            virtual void prepare() = 0;

            /**
             * Estimates the model's parameters from training data.
             */
            virtual void fit(const EvidenceSet& training_data) = 0;

            /**
             * Writes information about the splitter in a json object.
             *
             * @param json: json object
             */
            virtual void get_info(nlohmann::json& json) const = 0;

        };

    } // namespace model
} // namespace tomcat
