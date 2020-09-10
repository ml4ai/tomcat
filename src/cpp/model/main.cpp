#include "Tomcat.h"

#include <fstream>
#include <sstream>

#include <boost/program_options.hpp>

#include "model/pgm/EvidenceSet.h"
#include "model/pipeline/DBNSaver.h"
#include "model/pipeline/KFold.h"
#include "model/pipeline/Pipeline.h"
#include "model/pipeline/estimation/OfflineEstimation.h"
#include "model/pipeline/estimation/SumProductEstimator.h"
#include "model/pipeline/estimation/TrainingFrequencyEstimator.h"
#include "model/pipeline/evaluation/Accuracy.h"
#include "model/pipeline/evaluation/Estimates.h"
#include "model/pipeline/evaluation/EvaluationAggregator.h"
#include "model/pipeline/evaluation/F1Score.h"
#include "model/pipeline/training/DBNLoader.h"
#include "model/pipeline/training/DBNSamplingTrainer.h"
#include "model/sampling/GibbsSampler.h"
#include "model/utils/Definitions.h"

using namespace tomcat::model;
using namespace std;
namespace po = boost::program_options;

/**
 * 5 Cross validation with the falcon map in the engineering data.
 */
void execute_experiment_1a(string data_dir, string output_dir, string output_json) {
    cout << "Running experiment 1a" << endl;
    // Random Seed
    shared_ptr<gsl_rng> gen(gsl_rng_alloc(gsl_rng_mt19937));

    // Model
    Tomcat tomcat;
    tomcat.init_ta3_learnable_model();

    // Data
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
    shared_ptr<DBNSaver> saver = make_shared<DBNSaver>(DBNSaver(
        tomcat.get_model(), output_dir+"/fold{}"));

    // Estimation and evaluation
    shared_ptr<OfflineEstimation> offline_estimation =
        make_shared<OfflineEstimation>();

    shared_ptr<EvaluationAggregator> aggregator =
        make_shared<EvaluationAggregator>(
            EvaluationAggregator::METHOD::no_aggregation);

    vector<int> horizons = {1, 3, 5, 10, 15, 30};
    for (int horizon : horizons) {
        cout << "Horizon: " << horizon << endl;
        shared_ptr<Estimator> estimator =
            make_shared<TrainingFrequencyEstimator>(tomcat.get_model(),
                                                    horizon);
        estimator->add_node(Tomcat::SG, Eigen::VectorXd::Constant(1, 1));
        estimator->add_node(Tomcat::SY, Eigen::VectorXd::Constant(1, 1));
        offline_estimation->add_estimator(estimator);
        aggregator->add_measure(make_shared<Accuracy>(estimator));
        aggregator->add_measure(make_shared<F1Score>(estimator));

        estimator =
            make_shared<SumProductEstimator>(tomcat.get_model(), horizon);
        estimator->add_node(Tomcat::SG, Eigen::VectorXd::Constant(1, 1));
        estimator->add_node(Tomcat::SY, Eigen::VectorXd::Constant(1, 1));
        offline_estimation->add_estimator(estimator);
        aggregator->add_measure(make_shared<Accuracy>(estimator));
        aggregator->add_measure(make_shared<F1Score>(estimator));
        aggregator->add_measure(make_shared<Estimates>(estimator));
    }

    ofstream output_file;
    output_file.open(output_json);
    Pipeline pipeline("experiment_1a", output_file);
    pipeline.set_data_splitter(data_splitter);
    pipeline.set_model_trainer(trainer);
    pipeline.set_model_saver(saver);
    pipeline.set_estimation_process(offline_estimation);
    pipeline.set_aggregator(aggregator);
    pipeline.execute();
}

/**
 * Training with the engineering data and test with the human subject data
 */
void execute_experiment_1b() {
    cout << "Running experiment 1b" << endl;
    // Random Seed
    shared_ptr<gsl_rng> gen(gsl_rng_alloc(gsl_rng_mt19937));

    // Model
    Tomcat tomcat;
    tomcat.init_ta3_learnable_model();

    // Data
    EvidenceSet training_data("../../data/samples/ta3/falcon/engineering");
    EvidenceSet test_data("../../data/samples/ta3/falcon/human");

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
    shared_ptr<DBNSaver> saver = make_shared<DBNSaver>(
        DBNSaver(tomcat.get_model(), "../../data/model/ta3/experiment_1b/"));

    // Estimation and evaluation
    shared_ptr<OfflineEstimation> offline_estimation =
        make_shared<OfflineEstimation>();

    shared_ptr<EvaluationAggregator> aggregator =
        make_shared<EvaluationAggregator>(
            EvaluationAggregator::METHOD::no_aggregation);

    vector<int> horizons = {1}; //, 3, 5, 10, 15, 30};
    for (int horizon : horizons) {
        shared_ptr<Estimator> estimator =
            make_shared<TrainingFrequencyEstimator>(tomcat.get_model(),
                                                    horizon);
        estimator->add_node(Tomcat::SG, Eigen::VectorXd::Constant(1, 1));
        estimator->add_node(Tomcat::SY, Eigen::VectorXd::Constant(1, 1));
        offline_estimation->add_estimator(estimator);
        aggregator->add_measure(make_shared<Accuracy>(estimator));
        aggregator->add_measure(make_shared<F1Score>(estimator));

        estimator =
            make_shared<SumProductEstimator>(tomcat.get_model(), horizon);
        estimator->add_node(Tomcat::SG, Eigen::VectorXd::Constant(1, 1));
        estimator->add_node(Tomcat::SY, Eigen::VectorXd::Constant(1, 1));
        offline_estimation->add_estimator(estimator);
        aggregator->add_measure(make_shared<Accuracy>(estimator));
        aggregator->add_measure(make_shared<F1Score>(estimator));
        aggregator->add_measure(make_shared<Estimates>(estimator));
    }

    ofstream output_file;
    output_file.open(
        "../../data/evaluations/ta3/experiment_1b/evaluations.json");
    Pipeline pipeline("experiment_1b", output_file);
    pipeline.set_data_splitter(data_splitter);
    pipeline.set_model_trainer(trainer);
    pipeline.set_model_saver(saver);
    pipeline.set_estimation_process(offline_estimation);
    pipeline.set_aggregator(aggregator);
    pipeline.execute();
}

/**
 * Generates synthetic data using the trained model from experiment 1b with
 * equal samples up to time 100.
 */
void execute_experiment_1c_part_a() {
    cout << "Running experiment 1c part (a)" << endl;
    // Random Seed
    shared_ptr<gsl_rng> gen(gsl_rng_alloc(gsl_rng_mt19937));

    // Model
    Tomcat tomcat;
    tomcat.init_ta3_learnable_model();

    // Training
    shared_ptr<DBNTrainer> loader = make_shared<DBNLoader>(
        DBNLoader(tomcat.get_model(), "../../data/model/ta3/experiment_1b/"));
    loader->fit({});

    int num_samples = 10000;
    int equal_until_time_step = 100;
    tomcat.generate_synthetic_data(
        gen,
        num_samples,
        "../../data/samples/ta3/falcon/synthetic/experiment_1c",
        equal_until_time_step);
}

/**
 * Computes estimates for several values of inference horizons over the samples
 * generated in part a.
 */
void execute_experiment_1c_part_b() {
    cout << "Running experiment 1c part (b)" << endl;
    // Random Seed
    shared_ptr<gsl_rng> gen(gsl_rng_alloc(gsl_rng_mt19937));

    // Model
    Tomcat tomcat;
    tomcat.init_ta3_learnable_model();

    // Data
    EvidenceSet training_data; // Empty
    EvidenceSet test_data(
        "../../data/samples/ta3/falcon/synthetic/experiment_1c");
    test_data.keep_first(1);
    test_data.shrink_up_to(100);

    // Data split
    shared_ptr<KFold> data_splitter =
        make_shared<KFold>(training_data, test_data);

    // Training
    shared_ptr<DBNTrainer> loader = make_shared<DBNLoader>(
        DBNLoader(tomcat.get_model(), "../../data/model/ta3/experiment_1b/"));
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
        estimator->add_node(Tomcat::SG, Eigen::VectorXd::Constant(1, 1));
        estimator->add_node(Tomcat::SY, Eigen::VectorXd::Constant(1, 1));
        offline_estimation->add_estimator(estimator);
        aggregator->add_measure(make_shared<Estimates>(estimator));
    }

    ofstream output_file;
    output_file.open(
        "../../data/evaluations/ta3/experiment_1c/evaluations.json");
    Pipeline pipeline("experiment_1c", output_file);
    pipeline.set_data_splitter(data_splitter);
    pipeline.set_model_trainer(loader);
    pipeline.set_estimation_process(offline_estimation);
    pipeline.set_aggregator(aggregator);
    pipeline.execute();
    output_file.close();
}

/**
 * Chooses a different threshold for prediction of an assignment occurrence in
 * the inference horizon using the model trained in the experiment 1. The
 * threshold is 0.5 by default.
 */
void execute_experiment_1d(string data_dir, string output_dir) {
    cout << "Running experiment 1d" << endl;
    // Random Seed
    shared_ptr<gsl_rng> gen(gsl_rng_alloc(gsl_rng_mt19937));

    // Model
    Tomcat tomcat;
    tomcat.init_ta3_learnable_model();

    // Data
    EvidenceSet data(data_dir);

    // Data split
    int num_folds = 5;
    shared_ptr<KFold> data_splitter = make_shared<KFold>(data, num_folds, gen);

    // Training
    shared_ptr<DBNTrainer> loader = make_shared<DBNLoader>(
        DBNLoader(tomcat.get_model(), output_dir+"/fold{}"));

    vector<double> thresholds = {0.2, 0.3, 0.4, 0.5, 0.6, 0.7};
    vector<int> horizons = {1, 3, 5, 10, 15, 30};
    for (double threshold : thresholds) {
        cout << "Threshold: " << threshold << endl;
        // Estimation and evaluation
        shared_ptr<OfflineEstimation> offline_estimation =
            make_shared<OfflineEstimation>();

        shared_ptr<EvaluationAggregator> aggregator =
            make_shared<EvaluationAggregator>(
                EvaluationAggregator::METHOD::no_aggregation);

        for (int horizon : horizons) {
            cout << "Horizon: " << horizon << endl;
            shared_ptr<Estimator> estimator =
                make_shared<TrainingFrequencyEstimator>(tomcat.get_model(),
                                                        horizon);
            estimator->add_node(Tomcat::SG, Eigen::VectorXd::Constant(1, 1));
            estimator->add_node(Tomcat::SY, Eigen::VectorXd::Constant(1, 1));
            offline_estimation->add_estimator(estimator);
            aggregator->add_measure(make_shared<Accuracy>(estimator, threshold));
            aggregator->add_measure(make_shared<F1Score>(estimator, threshold));

            estimator =
                make_shared<SumProductEstimator>(tomcat.get_model(), horizon);
            estimator->add_node(Tomcat::SG, Eigen::VectorXd::Constant(1, 1));
            estimator->add_node(Tomcat::SY, Eigen::VectorXd::Constant(1, 1));
            offline_estimation->add_estimator(estimator);
            aggregator->add_measure(make_shared<Accuracy>(estimator, threshold));
            aggregator->add_measure(make_shared<F1Score>(estimator, threshold));
        }

        stringstream filepath;
        filepath << output_dir << "/evaluations_" << threshold << ".json";
        ofstream output_file;
        output_file.open(filepath.str());
        Pipeline pipeline("experiment_1d", output_file);
        pipeline.set_data_splitter(data_splitter);
        pipeline.set_model_trainer(loader);
        pipeline.set_estimation_process(offline_estimation);
        pipeline.set_aggregator(aggregator);
        pipeline.execute();
    }
}

void execute_experiment_1xxx() {
    //    MessageBrokerConfiguration config;
    //    config.timeout = 5;
    //    config.address = "localhost";
    //    config.port = 1883;
    //    shared_ptr<OnlineEstimation> online_estimation =
    //        make_shared<OnlineEstimation>(config);
    //    online_estimation->add_estimator(baseline_estimator);
}

int main(int argc, char* argv[]) {
    //execute_experiment_1b();
    //execute_experiment_1c_part_a();
    //execute_experiment_1c_part_b();

    string data_dir, output_dir, evaluation_json;
    po::options_description desc("Allowed options");
    desc.add_options()
        ("help,h", "Produce this help message")
        ("data_dir", po::value<string>(&data_dir)->default_value("../../data/samples/ta3/falcon/engineering"), "Data directory")
        ("output_dir", po::value<string>(&output_dir)->default_value("../../data/model/ta3/experiment_1a/"), "Output directory")
        ("evaluation_json",
         po::value<string>(&evaluation_json)->default_value("../../data/evaluations/ta3/experiment_1a/evaluations.json"),
         "Output directory")
    ;

    po::variables_map vm;
    po::store(po::parse_command_line(argc, argv, desc), vm);
    po::notify(vm);
    if (vm.count("help")) {
        cout << desc << "\n";
        return 1;
    }

    execute_experiment_1a(data_dir, output_dir, evaluation_json);
    execute_experiment_1d(data_dir, output_dir);
}
