#pragma once

#include <iostream>
#include <memory>
#include <string>
#include <vector>

#include <boost/date_time/posix_time/posix_time.hpp>

#include "pgm/EvidenceSet.h"
#include "utils/Definitions.h"
#include "pipeline/DBNSaver.h"
#include "pipeline/KFold.h"
#include "pipeline/estimation/EstimationProcess.h"
#include "pipeline/evaluation/EvaluationAggregator.h"
#include "pipeline/training/DBNTrainer.h"

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
            Pipeline(std::ostream& output_stream = std::cout);

            /**
             * Creates a pipeline with an associated id;
             *
             * @param id: pipeline's id for tracking.
             */
            Pipeline(const std::string& id,
                     std::ostream& output_stream = std::cout);

            ~Pipeline();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------

            // A pipeline cannot be copied or moved. This decision is just to
            // avoid implementing an explicit copy of the data members to deal
            // with the reference to the output stream. Given that a pipeline is
            // just a wrapper for running experiments, it's rarely there will be
            // a need for copying or move that would justify to make it
            // possible.
            Pipeline(const Pipeline&) = delete;

            Pipeline& operator=(const Pipeline&) = delete;

            Pipeline(Pipeline&&) = delete;

            Pipeline& operator=(Pipeline&&) = delete;

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Executes the pipeline. This operation splits the data set, trains
             * a model, saves it (optional), estimate values over the trained
             * model given unseen data, aggregate results (optional) and writes
             * the outcomes of the execution to an output stream.
             */
            void execute();

            /**
             * Writes information about the execution to an output stream.
             *
             * @param execution_start_time: time when the pipeline
             * execution started
             * @param execution_end_time: time when the pipeline
             * execution ended
             */
            void display_results(
                const boost::posix_time::ptime& execution_start_time,
                const boost::posix_time::ptime& execution_end_time);

            //------------------------------------------------------------------
            // Getters & Setters
            //------------------------------------------------------------------
            void set_data_splitter(const std::shared_ptr<KFold>& data_splitter);

            void set_model_trainer(
                const std::shared_ptr<DBNTrainer>& model_trainer);

            void set_model_saver(const std::shared_ptr<DBNSaver>& model_saver);

            void set_estimation_process(
                const std::shared_ptr<EstimationProcess>& estimation_process);

            void set_aggregator(
                const std::shared_ptr<EvaluationAggregator>& aggregator);

          private:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Checks if the pipeline is consistent, that is, whether at least a
             * data splitter and a  model trainer were provided.
             */
            void check();

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------
            std::string id;

            // Where to display pipeline execution's results.
            std::ostream& output_stream;

            std::shared_ptr<KFold> data_splitter;

            std::shared_ptr<DBNTrainer> model_trainer;

            std::shared_ptr<DBNSaver> model_saver;

            std::shared_ptr<EstimationProcess> estimation_process;

            std::shared_ptr<EvaluationAggregator> evaluation;
        };

    } // namespace model
} // namespace tomcat
