#pragma once

#include <memory>
#include <vector>
#include <string>

#include "DBNSaver.h"
#include "DBNTrainer.h"
#include "Estimation.h"
#include "EvidenceSet.h"
#include "KFold.h"
#include "MeasureAggregator.h"

namespace tomcat {
    namespace model {

        /**
         * This class represents a pipeline where a model can be trained and
         * tested according to some training procedure and evaluation metrics. A
         * pipeline can also be used to publish estimates from a pre-trained
         * model on the fly as data comes in real time. This can be achieved by
         * using an online estimation process in the pipeline. This feature is
         * one of the cores of the ToMCAT's communication system as other
         * modules can keep track of ToMCAT's calculations on the fly.
         *
         */
        class Pipeline {
          public:
            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates a pipeline with a randomly generated id;
             */
            Pipeline();

            /**
             * Creates a pipeline with an associated id;
             *
             * @param id: pipeline's id for tracking.
             */
            Pipeline(const std::string& id);

            ~Pipeline();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------
            Pipeline(const Pipeline&) = default;

            Pipeline& operator=(const Pipeline&) = default;

            Pipeline(Pipeline&&) = default;

            Pipeline& operator=(Pipeline&&) = default;

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------
            void execute();

          private:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------
            void start_estimation_threads(const EvidenceSet& test_data);

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------
            std::string id;

            EvidenceSet data;

            std::shared_ptr<KFold> data_splitter;

            std::shared_ptr<DBNTrainer> model_trainner;

            std::shared_ptr<DBNSaver> model_saver;

            std::vector<std::shared_ptr<Estimation>> estimations;

            std::shared_ptr<MeasureAggregator> aggregator;
        };

    } // namespace model
} // namespace tomcat
