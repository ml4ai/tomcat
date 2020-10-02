#include <fstream>
#include <sstream>

#include <boost/program_options.hpp>
#include <fmt/format.h>

#include "TomcatTA3.h"
#include "TomcatTA3V2.h"
#include "model/converter/TA3MessageConverter.h"
#include "pgm/EvidenceSet.h"
#include "pipeline/DBNSaver.h"
#include "pipeline/KFold.h"
#include "pipeline/Pipeline.h"
#include "pipeline/estimation/OfflineEstimation.h"
#include "pipeline/estimation/OnlineEstimation.h"
#include "pipeline/estimation/SumProductEstimator.h"
#include "pipeline/estimation/TrainingFrequencyEstimator.h"
#include "pipeline/evaluation/Accuracy.h"
#include "pipeline/evaluation/Estimates.h"
#include "pipeline/evaluation/EvaluationAggregator.h"
#include "pipeline/evaluation/F1Score.h"
#include "pipeline/training/DBNLoader.h"
#include "pipeline/training/DBNSamplingTrainer.h"
#include "sampling/GibbsSampler.h"
#include "utils/Definitions.h"

using namespace tomcat::model;
using namespace std;
namespace po = boost::program_options;

string DATA_ROOT_DIR = "../../data/samples";
string OUTPUT_ROOT_DIR = "../../data/";

/**
 * This source file implements the experiments described in details in the
 * document experimentation.tex.
 */

/**
 * Cross validation with the falcon map on engineering data.
 */
void execute_experiment_1a() {
    // Random Seed
    shared_ptr<gsl_rng> gen(gsl_rng_alloc(gsl_rng_mt19937));

    // Model
    TomcatTA3 tomcat;
    tomcat.init();

    // Data
    string data_dir = fmt::format("{}/ta3/falcon/engineering", DATA_ROOT_DIR);
    EvidenceSet data(data_dir);

    // Data split
    int num_folds = 5;
    shared_ptr<KFold> data_splitter = make_shared<KFold>(data, num_folds, gen);

    // Training
    int burn_in = 50;
    int num_samples = 100;
    shared_ptr<DBNTrainer> trainer = make_shared<DBNSamplingTrainer>(
        gen,
        make_shared<GibbsSampler>(tomcat.get_model(), burn_in),
        num_samples);

    // Saving
    string model_dir =
        fmt::format("{}/model/ta3/experiment_1a/fold{{}}", OUTPUT_ROOT_DIR);
    shared_ptr<DBNSaver> saver =
        make_shared<DBNSaver>(DBNSaver(tomcat.get_model(), model_dir));

    // Estimation and evaluation
    shared_ptr<OfflineEstimation> offline_estimation =
        make_shared<OfflineEstimation>();

    shared_ptr<EvaluationAggregator> aggregator =
        make_shared<EvaluationAggregator>(
            EvaluationAggregator::METHOD::no_aggregation);

    vector<int> horizons = {1, 3, 5, 10, 15, 30};
    for (int horizon : horizons) {
        shared_ptr<Estimator> estimator =
            make_shared<TrainingFrequencyEstimator>(tomcat.get_model(),
                                                    horizon);
        estimator->add_node(TomcatTA3::SG, Eigen::VectorXd::Constant(1, 1));
        estimator->add_node(TomcatTA3::SY, Eigen::VectorXd::Constant(1, 1));
        offline_estimation->add_estimator(estimator);
        aggregator->add_measure(make_shared<Accuracy>(estimator));
        aggregator->add_measure(make_shared<F1Score>(estimator));

        estimator =
            make_shared<SumProductEstimator>(tomcat.get_model(), horizon);
        estimator->add_node(TomcatTA3::SG, Eigen::VectorXd::Constant(1, 1));
        estimator->add_node(TomcatTA3::SY, Eigen::VectorXd::Constant(1, 1));
        offline_estimation->add_estimator(estimator);
        aggregator->add_measure(make_shared<Accuracy>(estimator));
        aggregator->add_measure(make_shared<F1Score>(estimator));
        aggregator->add_measure(make_shared<Estimates>(estimator));
    }

    ofstream output_file;
    string filepath = fmt::format(
        "{}/evaluations/ta3/experiment_1a/evaluations.json", OUTPUT_ROOT_DIR);
    output_file.open(filepath);
    Pipeline pipeline("experiment_1a", output_file);
    pipeline.set_data_splitter(data_splitter);
    pipeline.set_model_trainer(trainer);
    pipeline.set_model_saver(saver);
    pipeline.set_estimation_process(offline_estimation);
    pipeline.set_aggregator(aggregator);
    pipeline.execute();
}

/**
 * Training with engineering data and testing on human subject data
 */
void execute_experiment_1b() {
    // Random Seed
    shared_ptr<gsl_rng> gen(gsl_rng_alloc(gsl_rng_mt19937));

    // Model
    TomcatTA3 tomcat;
    tomcat.init();

    // Data
    string data_dir = fmt::format("{}/ta3/falcon/engineering", DATA_ROOT_DIR);
    EvidenceSet training_data(data_dir);
    data_dir = fmt::format("{}/ta3/falcon/human", DATA_ROOT_DIR);
    EvidenceSet test_data(data_dir);

    // Data split
    shared_ptr<KFold> data_splitter =
        make_shared<KFold>(training_data, test_data);

    // Training
    int burn_in = 50;
    int num_samples = 100;
    shared_ptr<DBNSamplingTrainer> trainer =
        make_shared<DBNSamplingTrainer>(DBNSamplingTrainer(
            gen,
            make_shared<GibbsSampler>(tomcat.get_model(), burn_in),
            num_samples));

    // Saving
    string model_dir =
        fmt::format("{}/model/ta3/experiment_1b/", OUTPUT_ROOT_DIR);
    shared_ptr<DBNSaver> saver =
        make_shared<DBNSaver>(DBNSaver(tomcat.get_model(), model_dir));

    // Estimation and evaluation
    shared_ptr<OfflineEstimation> offline_estimation =
        make_shared<OfflineEstimation>();

    shared_ptr<EvaluationAggregator> aggregator =
        make_shared<EvaluationAggregator>(
            EvaluationAggregator::METHOD::no_aggregation);

    vector<int> horizons = {1, 3, 5, 10, 15, 30};
    for (int horizon : horizons) {
        shared_ptr<Estimator> estimator =
            make_shared<TrainingFrequencyEstimator>(tomcat.get_model(),
                                                    horizon);
        estimator->add_node(TomcatTA3::SG, Eigen::VectorXd::Constant(1, 1));
        estimator->add_node(TomcatTA3::SY, Eigen::VectorXd::Constant(1, 1));
        offline_estimation->add_estimator(estimator);
        aggregator->add_measure(make_shared<Accuracy>(estimator));
        aggregator->add_measure(make_shared<F1Score>(estimator));

        estimator =
            make_shared<SumProductEstimator>(tomcat.get_model(), horizon);
        estimator->add_node(TomcatTA3::SG, Eigen::VectorXd::Constant(1, 1));
        estimator->add_node(TomcatTA3::SY, Eigen::VectorXd::Constant(1, 1));
        offline_estimation->add_estimator(estimator);
        aggregator->add_measure(make_shared<Accuracy>(estimator));
        aggregator->add_measure(make_shared<F1Score>(estimator));
        aggregator->add_measure(make_shared<Estimates>(estimator));
    }

    ofstream output_file;
    string filepath = fmt::format(
        "{}/evaluations/ta3/experiment_1b/evaluations.json", OUTPUT_ROOT_DIR);
    output_file.open(filepath);
    Pipeline pipeline("experiment_1b", output_file);
    pipeline.set_data_splitter(data_splitter);
    pipeline.set_model_trainer(trainer);
    pipeline.set_model_saver(saver);
    pipeline.set_estimation_process(offline_estimation);
    pipeline.set_aggregator(aggregator);
    pipeline.execute();
}

/**
 * Same as exmperiment 1a but assuming different thresholds for discretizing an
 * estimate to 0 or 1.
 */
void execute_experiment_1c() {
    // Random Seed
    shared_ptr<gsl_rng> gen(gsl_rng_alloc(gsl_rng_mt19937));

    // Model
    TomcatTA3 tomcat;
    tomcat.init();

    // Data
    string data_dir = fmt::format("{}/ta3/falcon/engineering", DATA_ROOT_DIR);
    EvidenceSet data(data_dir);

    // Data split
    int num_folds = 5;
    shared_ptr<KFold> data_splitter = make_shared<KFold>(data, num_folds, gen);

    // Training
    string model_dir =
        fmt::format("{}/model/ta3/experiment_1a/fold{{}}", OUTPUT_ROOT_DIR);
    shared_ptr<DBNTrainer> loader =
        make_shared<DBNLoader>(DBNLoader(tomcat.get_model(), model_dir));

    vector<double> thresholds = {0.2, 0.3, 0.4, 0.5, 0.6, 0.7};
    vector<int> horizons = {1, 3, 5, 10, 15, 30};
    for (double threshold : thresholds) {
        // Estimation and evaluation
        shared_ptr<OfflineEstimation> offline_estimation =
            make_shared<OfflineEstimation>();

        shared_ptr<EvaluationAggregator> aggregator =
            make_shared<EvaluationAggregator>(
                EvaluationAggregator::METHOD::no_aggregation);

        for (int horizon : horizons) {
            shared_ptr<Estimator> estimator =
                make_shared<TrainingFrequencyEstimator>(tomcat.get_model(),
                                                        horizon);
            estimator->add_node(TomcatTA3::SG, Eigen::VectorXd::Constant(1, 1));
            estimator->add_node(TomcatTA3::SY, Eigen::VectorXd::Constant(1, 1));
            offline_estimation->add_estimator(estimator);
            aggregator->add_measure(
                make_shared<Accuracy>(estimator, threshold));
            aggregator->add_measure(make_shared<F1Score>(estimator, threshold));

            estimator =
                make_shared<SumProductEstimator>(tomcat.get_model(), horizon);
            estimator->add_node(TomcatTA3::SG, Eigen::VectorXd::Constant(1, 1));
            estimator->add_node(TomcatTA3::SY, Eigen::VectorXd::Constant(1, 1));
            offline_estimation->add_estimator(estimator);
            aggregator->add_measure(
                make_shared<Accuracy>(estimator, threshold));
            aggregator->add_measure(make_shared<F1Score>(estimator, threshold));
        }

        ofstream output_file;
        string filepath =
            fmt::format("{}/evaluations/ta3/experiment_1a/evaluations_{}.json",
                        OUTPUT_ROOT_DIR,
                        threshold);
        output_file.open(filepath);
        Pipeline pipeline("experiment_1c", output_file);
        pipeline.set_data_splitter(data_splitter);
        pipeline.set_model_trainer(loader);
        pipeline.set_estimation_process(offline_estimation);
        pipeline.set_aggregator(aggregator);
        pipeline.execute();
    }
}

/**
 * Generates synthetic data using the trained model from experiment 1b with
 * equal samples up to time 100.
 */
void execute_experiment_1d_part_a() {
    // Random Seed
    shared_ptr<gsl_rng> gen(gsl_rng_alloc(gsl_rng_mt19937));

    // Model
    TomcatTA3 tomcat;
    tomcat.init();

    // Training
    string model_dir =
        fmt::format("{}/model/ta3/experiment_1b", OUTPUT_ROOT_DIR);
    shared_ptr<DBNTrainer> loader =
        make_shared<DBNLoader>(DBNLoader(tomcat.get_model(), model_dir));
    loader->fit({});

    int num_samples = 10000;
    int equal_until_time_step = 100;
    string samples_dir = fmt::format(
        "{}/samples/ta3/falcon/synthetic/experiment_1d", OUTPUT_ROOT_DIR);
    tomcat.generate_synthetic_data(
        gen, num_samples, samples_dir, equal_until_time_step);
}

/**
 * Computes estimates for several values of inference horizons over the samples
 * generated in part a. Samples are shrunk up to time step 100 as we are
 * interested in using the predictions at this time step.
 */
void execute_experiment_1d_part_b() {
    // Random Seed
    shared_ptr<gsl_rng> gen(gsl_rng_alloc(gsl_rng_mt19937));

    // Model
    TomcatTA3 tomcat;
    tomcat.init();

    // Data
    EvidenceSet training_data; // Empty
    string data_dir =
        fmt::format("{}/ta3/falcon/synthetic/experiment_1d", DATA_ROOT_DIR);
    EvidenceSet test_data(data_dir);
    test_data.keep_first(1);
    test_data.shrink_up_to(100);

    // Data split
    shared_ptr<KFold> data_splitter =
        make_shared<KFold>(training_data, test_data);

    // Training
    string model_dir =
        fmt::format("{}/model/ta3/experiment_1b", OUTPUT_ROOT_DIR);
    shared_ptr<DBNTrainer> loader =
        make_shared<DBNLoader>(DBNLoader(tomcat.get_model(), model_dir));
    loader->fit({});

    // Estimation and evaluation
    shared_ptr<OfflineEstimation> offline_estimation =
        make_shared<OfflineEstimation>();

    shared_ptr<EvaluationAggregator> aggregator =
        make_shared<EvaluationAggregator>(
            EvaluationAggregator::METHOD::no_aggregation);

    vector<int> horizons = {1, 3, 5, 10, 15, 30};
    for (int horizon : horizons) {
        shared_ptr<Estimator> estimator =
            make_shared<SumProductEstimator>(tomcat.get_model(), horizon);
        estimator->add_node(TomcatTA3::SG, Eigen::VectorXd::Constant(1, 1));
        estimator->add_node(TomcatTA3::SY, Eigen::VectorXd::Constant(1, 1));
        offline_estimation->add_estimator(estimator);
        aggregator->add_measure(make_shared<Estimates>(estimator));
    }

    ofstream output_file;
    string filepath = fmt::format(
        "{}/evaluations/ta3/experiment_1d/evaluations.json", OUTPUT_ROOT_DIR);
    output_file.open(filepath);
    Pipeline pipeline("experiment_1d", output_file);
    pipeline.set_data_splitter(data_splitter);
    pipeline.set_model_trainer(loader);
    pipeline.set_estimation_process(offline_estimation);
    pipeline.set_aggregator(aggregator);
    pipeline.execute();
    output_file.close();
}

/**
 * Generate samples with homogeneous world for each one of the possible time
 * steps in ht emodel.
 */
void execute_experiment_1e_part_a() {
    // Model
    TomcatTA3 tomcat;
    tomcat.init();

    // Training
    string model_dir =
        fmt::format("{}/model/ta3/experiment_1b", OUTPUT_ROOT_DIR);
    shared_ptr<DBNTrainer> loader =
        make_shared<DBNLoader>(DBNLoader(tomcat.get_model(), model_dir));
    loader->fit({});

    int num_samples = 1000;

    for (int t = 0; t < tomcat.get_model()->get_time_steps(); t++) {
        LOG(t);
        // Random Seed
        shared_ptr<gsl_rng> gen(gsl_rng_alloc(gsl_rng_mt19937));

        string samples_dir = fmt::format("{}/samples/ta3/falcon/synthetic/"
                                         "experiment_1e/homogeneous_up_to_{}",
                                         OUTPUT_ROOT_DIR,
                                         t);

        // Extend the max time step so that empirical probabilities can be
        // estimated in a window as large as 100 time steps.
        tomcat.generate_synthetic_data(
            gen, num_samples, samples_dir, t, t + 100);
    }
}

/**
 * Computes estimates over the last set of samples generated in part a. In that
 * scenario, the whole world was fixed so we just need to estimate for one data
 * sample over time (as they are all the same). The different homogeneous world
 * samples generated in part a will serve to compute the empirical
 * probabilities. We can use the last world configuration because the samples
 * were generated in a deterministic fashion given that the random seed was
 * always restarted before generating a new set of samples.
 */
void execute_experiment_1e_part_b() {
    // Model
    TomcatTA3 tomcat;
    tomcat.init();

    // Data
    EvidenceSet training_data; // Empty
    int last_time_step = tomcat.get_model()->get_time_steps() - 1;
    string data_dir = fmt::format(
        "{}/ta3/falcon/synthetic/experiment_1e/homogeneous_up_to_{}",
        DATA_ROOT_DIR,
        last_time_step);
    EvidenceSet test_data(data_dir);
    test_data.keep_first(1);

    // Data split
    shared_ptr<KFold> data_splitter =
        make_shared<KFold>(training_data, test_data);

    // Training
    string model_dir =
        fmt::format("{}/model/ta3/experiment_1b", OUTPUT_ROOT_DIR);
    shared_ptr<DBNTrainer> loader =
        make_shared<DBNLoader>(DBNLoader(tomcat.get_model(), model_dir));
    loader->fit({});

    // Estimation and evaluation
    shared_ptr<OfflineEstimation> offline_estimation =
        make_shared<OfflineEstimation>();

    shared_ptr<EvaluationAggregator> aggregator =
        make_shared<EvaluationAggregator>(
            EvaluationAggregator::METHOD::no_aggregation);

    vector<int> horizons = {1, 3, 5, 10, 15, 30};
    for (int horizon : horizons) {
        shared_ptr<Estimator> estimator =
            make_shared<SumProductEstimator>(tomcat.get_model(), horizon);
        estimator->add_node(TomcatTA3::SG, Eigen::VectorXd::Constant(1, 1));
        estimator->add_node(TomcatTA3::SY, Eigen::VectorXd::Constant(1, 1));
        offline_estimation->add_estimator(estimator);
        aggregator->add_measure(make_shared<Estimates>(estimator));
    }

    ofstream output_file;
    string filepath = fmt::format(
        "{}/evaluations/ta3/experiment_1e/evaluations.json", OUTPUT_ROOT_DIR);
    output_file.open(filepath);
    Pipeline pipeline("experiment_1e", output_file);
    pipeline.set_data_splitter(data_splitter);
    pipeline.set_model_trainer(loader);
    pipeline.set_estimation_process(offline_estimation);
    pipeline.set_aggregator(aggregator);
    pipeline.execute();
    output_file.close();
}

/**
 * Generates synthetic data the trained model from experiment 1b without
 * freezing the world.
 */
void execute_experiment_1f_part_a() {
    // Random Seed
    shared_ptr<gsl_rng> gen(gsl_rng_alloc(gsl_rng_mt19937));

    // Model
    TomcatTA3 tomcat;
    tomcat.init();

    // Training
    string model_dir =
        fmt::format("{}/model/ta3/experiment_1b", OUTPUT_ROOT_DIR);
    shared_ptr<DBNTrainer> loader =
        make_shared<DBNLoader>(DBNLoader(tomcat.get_model(), model_dir));
    loader->fit({});

    int num_samples = 1000;
    string samples_dir = fmt::format(
        "{}/samples/ta3/falcon/synthetic/experiment_1f", OUTPUT_ROOT_DIR);
    tomcat.generate_synthetic_data(gen, num_samples, samples_dir);
}

/**
 * Computes the performance of the model on synthetic data for several values of
 * the inference horizon.
 */
void execute_experiment_1f_part_b() {
    // Random Seed
    shared_ptr<gsl_rng> gen(gsl_rng_alloc(gsl_rng_mt19937));

    // Model
    TomcatTA3 tomcat;
    tomcat.init();

    // Data
    EvidenceSet training_data;
    string data_dir =
        fmt::format("{}/ta3/falcon/synthetic/experiment_1f", DATA_ROOT_DIR);
    EvidenceSet test_data(data_dir);

    // Data split
    shared_ptr<KFold> data_splitter =
        make_shared<KFold>(training_data, test_data);

    // Training
    string model_dir =
        fmt::format("{}/model/ta3/experiment_1b", OUTPUT_ROOT_DIR);
    shared_ptr<DBNTrainer> loader =
        make_shared<DBNLoader>(tomcat.get_model(), model_dir);

    // Estimation and evaluation
    shared_ptr<OfflineEstimation> offline_estimation =
        make_shared<OfflineEstimation>();

    shared_ptr<EvaluationAggregator> aggregator =
        make_shared<EvaluationAggregator>(
            EvaluationAggregator::METHOD::no_aggregation);

    vector<int> horizons = {1, 3, 5, 10, 15, 30};
    for (int horizon : horizons) {
        shared_ptr<Estimator> estimator =
            make_shared<SumProductEstimator>(tomcat.get_model(), horizon);
        estimator->add_node(TomcatTA3::SG, Eigen::VectorXd::Constant(1, 1));
        estimator->add_node(TomcatTA3::SY, Eigen::VectorXd::Constant(1, 1));
        offline_estimation->add_estimator(estimator);
        aggregator->add_measure(make_shared<Accuracy>(estimator));
        aggregator->add_measure(make_shared<F1Score>(estimator));
        aggregator->add_measure(make_shared<Estimates>(estimator));
    }

    ofstream output_file;
    string filepath = fmt::format(
        "{}/evaluations/ta3/experiment_1f/evaluations.json", OUTPUT_ROOT_DIR);
    output_file.open(filepath);
    Pipeline pipeline("experiment_1f", output_file);
    pipeline.set_data_splitter(data_splitter);
    pipeline.set_model_trainer(loader);
    pipeline.set_estimation_process(offline_estimation);
    pipeline.set_aggregator(aggregator);
    pipeline.execute();
}

/**
 * Generates samples for each one of the matrices computed in experiment 1a
 * using cross validation.
 */
void execute_experiment_1g_part_a() {
    // Random Seed
    shared_ptr<gsl_rng> gen(gsl_rng_alloc(gsl_rng_mt19937));

    // Model
    TomcatTA3 tomcat;
    tomcat.init();

    // Training
    for (int fold = 1; fold <= 5; fold++) {
        string model_dir = fmt::format(
            "{}/model/ta3/experiment_1a/fold{}", OUTPUT_ROOT_DIR, fold);
        shared_ptr<DBNTrainer> loader =
            make_shared<DBNLoader>(DBNLoader(tomcat.get_model(), model_dir));
        loader->fit({});

        int num_samples = 100;
        string samples_dir =
            fmt::format("{}/samples/ta3/falcon/synthetic/experiment_1g/fold{}",
                        OUTPUT_ROOT_DIR,
                        fold);
        tomcat.generate_synthetic_data(gen, num_samples, samples_dir);
    }
}

/**
 * Compute estimates for the samples generated in part a.
 */
void execute_experiment_1g_part_b() {
    // Model
    TomcatTA3 tomcat;
    tomcat.init();

    for (int fold = 1; fold <= 5; fold++) {
        std::cout << "Fold " << fold << std::endl;

        // Data
        EvidenceSet training_data; // Empty
        string data_dir =
            fmt::format("{}/ta3/falcon/synthetic/experiment_1g/fold{}",
                        DATA_ROOT_DIR,
                        fold);
        EvidenceSet test_data(data_dir);

        // Data split
        shared_ptr<KFold> data_splitter =
            make_shared<KFold>(training_data, test_data);

        // Training
        string model_dir = fmt::format(
            "{}/model/ta3/experiment_1a/fold{}", OUTPUT_ROOT_DIR, fold);
        shared_ptr<DBNTrainer> loader =
            make_shared<DBNLoader>(DBNLoader(tomcat.get_model(), model_dir));
        loader->fit({});

        // Estimation and evaluation
        shared_ptr<OfflineEstimation> offline_estimation =
            make_shared<OfflineEstimation>();

        shared_ptr<EvaluationAggregator> aggregator =
            make_shared<EvaluationAggregator>(
                EvaluationAggregator::METHOD::no_aggregation);

        vector<int> horizons = {1, 3, 5, 10, 15, 30};
        for (int horizon : horizons) {
            shared_ptr<Estimator> estimator =
                make_shared<SumProductEstimator>(tomcat.get_model(), horizon);
            estimator->add_node(TomcatTA3::SG, Eigen::VectorXd::Constant(1, 1));
            estimator->add_node(TomcatTA3::SY, Eigen::VectorXd::Constant(1, 1));
            offline_estimation->add_estimator(estimator);
            aggregator->add_measure(make_shared<Estimates>(estimator));
        }

        ofstream output_file;
        string filepath = fmt::format(
            "{}/evaluations/ta3/experiment_1g/fold{}/evaluations.json",
            OUTPUT_ROOT_DIR,
            fold);
        output_file.open(filepath);
        Pipeline pipeline("experiment_1g", output_file);
        pipeline.set_data_splitter(data_splitter);
        pipeline.set_model_trainer(loader);
        pipeline.set_estimation_process(offline_estimation);
        pipeline.set_aggregator(aggregator);
        pipeline.execute();
        output_file.close();
    }
}

void execute_experiment_2a() {
    // Random Seed
    shared_ptr<gsl_rng> gen(gsl_rng_alloc(gsl_rng_mt19937));

    // Model
    TomcatTA3V2 tomcat;
    tomcat.init();

    // Data
    string data_dir = fmt::format("{}/ta3/falcon/converter/train", DATA_ROOT_DIR);
    EvidenceSet training_data(data_dir);
    data_dir = fmt::format("{}/ta3/falcon/converter/test", DATA_ROOT_DIR);
    EvidenceSet test_data(data_dir);

    // Data split
    shared_ptr<KFold> data_splitter =
        make_shared<KFold>(training_data, test_data);

    // Training
//    int burn_in = 100;
//    int num_samples = 500;
//    shared_ptr<DBNSamplingTrainer> trainer =
//        make_shared<DBNSamplingTrainer>(DBNSamplingTrainer(
//            gen,
//            make_shared<GibbsSampler>(tomcat.get_model(), burn_in),
//            num_samples));
    string model_dir = fmt::format(
        "{}/model/ta3/experiment_2a", OUTPUT_ROOT_DIR);
    shared_ptr<DBNTrainer> loader =
        make_shared<DBNLoader>(DBNLoader(tomcat.get_model(), model_dir));
    loader->fit({});

    // Saving
//    string model_dir =
//        fmt::format("{}/model/ta3/experiment_2a/", OUTPUT_ROOT_DIR);
//    shared_ptr<DBNSaver> saver =
//        make_shared<DBNSaver>(DBNSaver(tomcat.get_model(), model_dir));

    // Estimation and evaluation
    shared_ptr<OfflineEstimation> offline_estimation =
        make_shared<OfflineEstimation>();

    shared_ptr<EvaluationAggregator> aggregator =
        make_shared<EvaluationAggregator>(
            EvaluationAggregator::METHOD::no_aggregation);

    vector<int> horizons = {1};//, 3, 5, 10, 15, 30};
    for (int horizon : horizons) {
        shared_ptr<Estimator> estimator =
            make_shared<SumProductEstimator>(tomcat.get_model(), horizon);
        estimator->add_node(TomcatTA3::SG, Eigen::VectorXd::Constant(1, 1));
        estimator->add_node(TomcatTA3::SY, Eigen::VectorXd::Constant(1, 1));
        offline_estimation->add_estimator(estimator);
        aggregator->add_measure(make_shared<Estimates>(estimator));
    }

    shared_ptr<Estimator> estimator =
        make_shared<SumProductEstimator>(tomcat.get_model(), 0);
    estimator->add_node(TomcatTA3V2::Q, Eigen::VectorXd::Constant(1, 0));
    estimator->add_node(TomcatTA3V2::Q, Eigen::VectorXd::Constant(1, 1));
    estimator->add_node(TomcatTA3V2::Q, Eigen::VectorXd::Constant(1, 2));
    estimator->add_node(TomcatTA3V2::Q, Eigen::VectorXd::Constant(1, 3));
    offline_estimation->add_estimator(estimator);
    aggregator->add_measure(make_shared<Estimates>(estimator));

    ofstream output_file;
    string filepath = fmt::format(
        "{}/evaluations/ta3/experiment_2a/evaluations.json", OUTPUT_ROOT_DIR);
    output_file.open(filepath);
    Pipeline pipeline("experiment_2a", output_file);
    pipeline.set_data_splitter(data_splitter);
    pipeline.set_model_trainer(loader);
//    pipeline.set_model_trainer(trainer);
//    pipeline.set_model_saver(saver);
    pipeline.set_estimation_process(offline_estimation);
    pipeline.set_aggregator(aggregator);
    pipeline.execute();
}

void execute_experiment_1xxx() {
    // Model
    TomcatTA3 tomcat;
    tomcat.init();

    // Data
    EvidenceSet training_data; // Empty
    EvidenceSet test_data;     // Empty

    // Data split
    shared_ptr<KFold> data_splitter =
        make_shared<KFold>(training_data, test_data);

    // Training
    string model_dir =
        fmt::format("{}/model/ta3/experiment_1b", OUTPUT_ROOT_DIR);
    shared_ptr<DBNTrainer> loader =
        make_shared<DBNLoader>(DBNLoader(tomcat.get_model(), model_dir));
    loader->fit({});

    // Estimation
    OnlineEstimation::MessageBrokerConfiguration config;
    config.timeout = 9999999;
    config.address = "localhost";
    config.port = 1883;

    shared_ptr<MessageConverter> converter = make_shared<TA3MessageConverter>(
        "../../data/maps/ta3/falcon_v1.0.json");
    shared_ptr<OnlineEstimation> online_estimation =
        make_shared<OnlineEstimation>(config, converter);

    shared_ptr<Estimator> estimator =
        make_shared<SumProductEstimator>(tomcat.get_model(), 1);
    estimator->add_node(TomcatTA3::SG, Eigen::VectorXd::Constant(1, 1));
    estimator->add_node(TomcatTA3::SY, Eigen::VectorXd::Constant(1, 1));
    online_estimation->add_estimator(estimator);

    Pipeline pipeline("experiment_1d");
    pipeline.set_data_splitter(data_splitter);
    pipeline.set_model_trainer(loader);
    pipeline.set_estimation_process(online_estimation);
    pipeline.execute();
}

void execute_experiment(const string& experiment_id) {
    if (experiment_id == "1a") {
        execute_experiment_1a();
    }
    else if (experiment_id == "1b") {
        execute_experiment_1b();
    }
    else if (experiment_id == "1c") {
        execute_experiment_1c();
    }
    else if (experiment_id == "1d") {
        execute_experiment_1d_part_a();
        execute_experiment_1d_part_b();
    }
    else if (experiment_id == "1d_a") {
        execute_experiment_1d_part_a();
    }
    else if (experiment_id == "1d_b") {
        execute_experiment_1d_part_b();
    }
    else if (experiment_id == "1e") {
        execute_experiment_1e_part_a();
        execute_experiment_1e_part_b();
    }
    else if (experiment_id == "1e_a") {
        execute_experiment_1e_part_a();
    }
    else if (experiment_id == "1e_b") {
        execute_experiment_1e_part_b();
    }
    else if (experiment_id == "1f") {
        execute_experiment_1f_part_a();
        execute_experiment_1f_part_b();
    }
    else if (experiment_id == "1f_a") {
        execute_experiment_1f_part_a();
    }
    else if (experiment_id == "1f_b") {
        execute_experiment_1f_part_b();
    }
    else {
        throw TomcatModelException(
            "There's no experiment with the informed ID.");
    }
}

int main(int argc, char* argv[]) {
    string experiment_id;
    po::options_description desc("Allowed options");
    desc.add_options()("help,h", "Produce this help message")(
        "input_dir",
        po::value<string>(&DATA_ROOT_DIR)->default_value("../../data/samples/"),
        "Directory where input data is.")(
        "output_dir",
        po::value<string>(&OUTPUT_ROOT_DIR)->default_value("../../data/"),
        "Output directory for generated data, model and evaluation.")(
        "experiment_id",
        po::value<string>(&experiment_id)->default_value("1a"),
        "Mission ID or path to mission XML file.\n"
        "  1a: ToMCAT-TA3 v1.0 - 5-CV on engineering data.\n"
        "  1b: ToMCAT-TA3 v1.0 - Trains on engineering data, tests on "
        "human "
        "data.\n"
        "  1c: ToMCAT-TA3 v1.0 - Experiment 1a with different thresholds.\n"
        "  1d: ToMCAT-TA3 v1.0 - Experiments 1d_a + 1d_b.\n"
        "  1d_a: ToMCAT-TA3 v1.0 - Generates samples with homogeneous "
        "world up "
        "to time step 100.\n"
        "  1d_b: ToMCAT-TA3 v1.0 - Evaluates predictions at time step 100 "
        "on "
        "data generated in part a.\n"
        "  1e: ToMCAT-TA3 v1.0 - Experiments 1e_a + 1e_b.\n"
        "  1e_a: ToMCAT-TA3 v1.0 - Generates samples with homogeneous "
        "world "
        "for all time steps.\n"
        "  1e_b: ToMCAT-TA3 v1.0 - Evaluates predictions at all time steps "
        "the "
        "data generated in part a.\n"
        "  1f: ToMCAT-TA3 v1.0 - Experiments 1f_a + 1f_b.\n"
        "  1f_a: ToMCAT-TA3 v1.0 - Generates data with no world "
        "constraints.\n"
        "  1f_b: ToMCAT-TA3 v1.0 - Evaluates predictions at all time steps "
        "the "
        "data generated in part a.\n");

    po::variables_map vm;
    po::store(po::parse_command_line(argc, argv, desc), vm);
    po::notify(vm);
    if (vm.count("help") || !vm.count("experiment_id")) {
        cout << desc << "\n";
        return 1;
    }

    execute_experiment(experiment_id);
      //execute_experiment_2a();

//    TomcatTA3V2 tomcat;
//    tomcat.init();
//    tomcat.get_model()->write_graphviz(cout);

    //execute_experiment_1xxx();

//    // Random Seed
//    shared_ptr<gsl_rng> gen(gsl_rng_alloc(gsl_rng_mt19937));
//
//    // Model
//    TomcatTA3 tomcat;
//    tomcat.init();
//
//    // Data
//    EvidenceSet training_data;
//    string data_dir =
//        fmt::format("{}/ta3/falcon/converter/", DATA_ROOT_DIR);
//    EvidenceSet test_data(data_dir);
//
//    // Data split
//    shared_ptr<KFold> data_splitter =
//        make_shared<KFold>(training_data, test_data);
//
//    // Training
//    string model_dir =
//        fmt::format("{}/model/ta3/experiment_1b", OUTPUT_ROOT_DIR);
//    shared_ptr<DBNTrainer> loader =
//        make_shared<DBNLoader>(tomcat.get_model(), model_dir);
//
//    // Estimation and evaluation
//    shared_ptr<OfflineEstimation> offline_estimation =
//        make_shared<OfflineEstimation>();
//
//    shared_ptr<EvaluationAggregator> aggregator =
//        make_shared<EvaluationAggregator>(
//            EvaluationAggregator::METHOD::no_aggregation);
//
//    vector<int> horizons = {1};
//    for (int horizon : horizons) {
//        shared_ptr<Estimator> estimator =
//            make_shared<SumProductEstimator>(tomcat.get_model(), horizon);
//        estimator->add_node(TomcatTA3::SG, Eigen::VectorXd::Constant(1, 1));
//        estimator->add_node(TomcatTA3::SY, Eigen::VectorXd::Constant(1, 1));
//        offline_estimation->add_estimator(estimator);
//        aggregator->add_measure(make_shared<Estimates>(estimator));
//    }
//
//    ofstream output_file;
//    string filepath = fmt::format(
//        "{}/evaluations/ta3/evaluations.json", OUTPUT_ROOT_DIR);
//    output_file.open(filepath);
//    Pipeline pipeline("online_check", output_file);
//    pipeline.set_data_splitter(data_splitter);
//    pipeline.set_model_trainer(loader);
//    pipeline.set_estimation_process(offline_estimation);
//    pipeline.set_aggregator(aggregator);
//    pipeline.execute();
}
