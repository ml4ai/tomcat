#pragma once

#include <memory>
#include <string>
#include <thread>
#include <vector>

#include "../pgm/DBNData.h"
#include "../utils/Definitions.h"
#include "DBNSaver.h"
#include "KFold.h"
#include "estimation/Estimation.h"
#include "evaluation/MeasureAggregator.h"
#include "training/DBNTrainer.h"

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

            void add_estimation(const std::shared_ptr<Estimation>& estimation);

            //------------------------------------------------------------------
            // Getters & Setters
            //------------------------------------------------------------------
            void set_data(const DBNData& data);

            void set_data_splitter(const std::shared_ptr<KFold>& data_splitter);

            void set_model_trainner(
                const std::shared_ptr<DBNTrainer>& model_trainner);

            void set_model_saver(const std::shared_ptr<DBNSaver>& model_saver);

            void set_aggregator(
                const std::shared_ptr<MeasureAggregator>& aggregator);

          private:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------
            std::vector<std::thread>
            start_estimation_threads(const DBNData& training_data,
                                     const DBNData& test_data);

            void estimate(std::shared_ptr<Estimation> estimation,
                          const DBNData& test_data);

            /**
             * Checks if the pipeline is consistent, that is, whether at least a
             * data splitter and a  model trainer were provided.
             */
            void check();

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------
            std::string id;

            DBNData data;

            std::shared_ptr<KFold> data_splitter;

            std::shared_ptr<DBNTrainer> model_trainner;

            std::shared_ptr<DBNSaver> model_saver;

            std::vector<std::shared_ptr<Estimation>> estimations;

            std::shared_ptr<MeasureAggregator> aggregator;
        };

    } // namespace model
} // namespace tomcat
