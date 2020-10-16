#pragma once

#include <memory>
#include <unordered_set>
#include <vector>
#include <string>

#include <eigen3/Eigen/Dense>
#include <fmt/format.h>
#include <gsl/gsl_rng.h>

#include "converter/TA3MessageConverter.h"
#include "experiments/TomcatTA3.h"
#include "experiments/TomcatTA3V2.h"
#include "pgm/EvidenceSet.h"
#include "pipeline/DBNSaver.h"
#include "pipeline/KFold.h"
#include "pipeline/Pipeline.h"
#include "pipeline/estimation/OfflineEstimation.h"
#include "pipeline/estimation/SumProductEstimator.h"
#include "pipeline/estimation/TrainingFrequencyEstimator.h"
#include "pipeline/evaluation/Accuracy.h"
#include "pipeline/evaluation/EvaluationAggregator.h"
#include "pipeline/evaluation/F1Score.h"
#include "pipeline/training/DBNLoader.h"
#include "pipeline/training/DBNSamplingTrainer.h"
#include "sampling/GibbsSampler.h"
#include "utils/Definitions.h"

namespace tomcat {
    namespace model {

        /**
         * Class description here
         */
        class Experimentation {
          public:
            //------------------------------------------------------------------
            // Types, Enums & Constants
            //------------------------------------------------------------------
            enum MODEL_VERSION { v1, v2 };
            enum MEASURES { accuracy, f1 };

            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Initializes an experiment with fixed training and test data.
             *
             * @param gen: random number generator
             * @param experiment_id: id of the experiment
             * @param model_version: version of the ToMCAT model
             * @param training_set: data used to learn the model's parameters
             * @param test_set: data used to evaluate the predictions and
             * inferences
             */
            Experimentation(std::shared_ptr<gsl_rng> gen,
                            const std::string& experiment_id,
                            MODEL_VERSION model_version,
                            const EvidenceSet& training_set,
                            const EvidenceSet& test_set);

            /**
             * Initializes an experiment using cross validation.
             *
             * @param gen: random number generator
             * @param experiment_id: id of the experiment
             * @param model_version: version of the ToMCAT model
             * @param data: data to be split using cross validation
             * @param num_folds: number of folds in the cross validation
             */
            Experimentation(std::shared_ptr<gsl_rng> gen,
                            const std::string& experiment_id,
                            MODEL_VERSION model_version,
                            const EvidenceSet& data,
                            int num_folds);

            /**
             * Initializes an experiment for synthetic data generation.
             *
             * @param gen: random number generator
             * @param model_version: version of the ToMCAT model
             */
            Experimentation(std::shared_ptr<gsl_rng> gen,
                            MODEL_VERSION model_version);

            ~Experimentation();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------
            Experimentation(const Experimentation&) = default;

            Experimentation& operator=(const Experimentation&) = default;

            Experimentation(Experimentation&&) = default;

            Experimentation& operator=(Experimentation&&) = default;

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------
            void display_estimates();

            void load_model_from(const std::string& input_dir);

            void train_using_gibbs(int burn_in, int num_samples);

            void save_model(const std::string& output_dir);

            void compute_baseline_estimates_for(
                const std::string& node_label,
                int inference_horizon,
                Eigen::VectorXd assignment = Eigen::VectorXd(0));

            void compute_estimates_for(
                const std::string& node_label,
                int inference_horizon,
                Eigen::VectorXd assignment = Eigen::VectorXd(0));

            void compute_baseline_eval_scores_for(
                const std::string& node_label,
                int inference_horizon,
                std::vector<MEASURES> measures,
                Eigen::VectorXd assignment = Eigen::VectorXd(0));

            void compute_eval_scores_for(
                const std::string& node_label,
                int inference_horizon,
                std::vector<MEASURES> measures,
                Eigen::VectorXd assignment = Eigen::VectorXd(0));

            void train_and_evaluate(const std::string& output_dir);

            void train_and_save();

            void generate_synthetic_data(int num_samples,
                                         const std::string& output_dir,
                                         int equals_until = 0,
                                         int max_time_step = -1);

          protected:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------

          private:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------
            void init_model(MODEL_VERSION model_version);

            void init_evaluation();

            bool should_eval_last_only(const std::string& node_label);

            void compute_eval_scores_for(const std::string& node_label,
                                         int inference_horizon,
                                         std::vector<MEASURES> measures,
                                         Eigen::VectorXd assignment,
                                         std::shared_ptr<Estimator> estimator);

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------
            std::shared_ptr<gsl_rng> gen;

            std::shared_ptr<Tomcat> tomcat;

            std::shared_ptr<KFold> data_splitter;

            std::shared_ptr<DBNTrainer> trainer;

            std::shared_ptr<DBNSaver> saver;

            std::shared_ptr<OfflineEstimation> offline_estimation;

            std::shared_ptr<EvaluationAggregator> evaluation;

            std::string experiment_id;

            // Labels of nodes to exclude when generating synthetic data (hidden
            // nodes that should not be used in the inference).
            std::unordered_set<std::string> data_generation_exclusions;
        };

    } // namespace model
} // namespace tomcat
