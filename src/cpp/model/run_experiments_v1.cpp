/**
 * This source file implements the experiments described in details in the
 * document experimentation.pdf for the version 1.0 of the ToMCAT model.
 */

#include <string>
#include <vector>

#include <boost/program_options.hpp>
#include <eigen3/Eigen/Dense>
#include <fmt/format.h>
#include <gsl/gsl_rng.h>

#include "experiments/Experimentation.h"
#include "experiments/TomcatTA3.h"
#include "pgm/EvidenceSet.h"

using namespace tomcat::model;
using namespace std;
namespace po = boost::program_options;

#define MODEL_VERSION Experimentation::MODEL_VERSION
#define MEASURES Experimentation::MEASURES

string DATA_ROOT_DIR = "../../data/samples";
string OUTPUT_ROOT_DIR = "../../data/";

/**
 * Performs a 5 cross validation on the falcon map using human data to predict
 * victim rescuing for several values of inference horizon.
 */
void execute_experiment_1a() {
    cout << "Experiment 1a\n";

    // Random Seed
    shared_ptr<gsl_rng> gen(gsl_rng_alloc(gsl_rng_mt19937));

    // Data
    string data_dir = fmt::format("{}/ta3/falcon/human", DATA_ROOT_DIR);
    EvidenceSet data(data_dir);

    Experimentation experimentation(gen, "1a", MODEL_VERSION::v1, data, 5);

    experimentation.train_using_gibbs(50, 100);
    string model_dir = fmt::format("{}/model/ta3/1a", OUTPUT_ROOT_DIR);
    experimentation.save_model(model_dir);

    vector<int> horizons = {1, 3, 5, 10, 15, 30, 50, 100};
    Eigen::VectorXd assignment = Eigen::VectorXd::Constant(1, 1);
    vector<MEASURES> measures = {MEASURES::accuracy, MEASURES::f1};
    for (int horizon : horizons) {
        experimentation.compute_baseline_eval_scores_for(
            TomcatTA3::SG, horizon, measures, assignment);
        experimentation.compute_baseline_eval_scores_for(
            TomcatTA3::SY, horizon, measures, assignment);

        experimentation.compute_eval_scores_for(
            TomcatTA3::SG, horizon, measures, assignment);
        experimentation.compute_eval_scores_for(
            TomcatTA3::SY, horizon, measures, assignment);
    }

    string evaluations_dir =
        fmt::format("{}/evaluations/ta3/1a", OUTPUT_ROOT_DIR);
    experimentation.display_estimates();
    experimentation.train_and_evaluate(evaluations_dir);
}

/**
 * Trains the model using the complete human data.
 */
void execute_experiment_1b_part_a() {
    cout << "Experiment 1b part a\n";

    // Random Seed
    shared_ptr<gsl_rng> gen(gsl_rng_alloc(gsl_rng_mt19937));

    // Data
    string data_dir = fmt::format("{}/ta3/falcon/human", DATA_ROOT_DIR);
    EvidenceSet training_set(data_dir);
    EvidenceSet test_set;

    Experimentation experimentation(
        gen, "1b", Experimentation::MODEL_VERSION::v1, training_set, test_set);

    experimentation.train_using_gibbs(50, 100);
    string model_dir = fmt::format("{}/model/ta3/1b", OUTPUT_ROOT_DIR);
    experimentation.save_model(model_dir);
    experimentation.train_and_save();
}

/**
 * Evaluates the prediction of victim rescuing, for several values of inference
 * horizon, on the model trained in part a using the data used to train it.
 */
void execute_experiment_1b_part_b() {
    cout << "Experiment 1b part b\n";

    // Random Seed
    shared_ptr<gsl_rng> gen(gsl_rng_alloc(gsl_rng_mt19937));

    // Data
    string data_dir = fmt::format("{}/ta3/falcon/human", DATA_ROOT_DIR);
    EvidenceSet training_set(data_dir);
    EvidenceSet test_set(data_dir);

    Experimentation experimentation(
        gen, "1b", Experimentation::MODEL_VERSION::v1, training_set, test_set);

    string model_dir = fmt::format("{}/model/ta3/1b", OUTPUT_ROOT_DIR);
    experimentation.load_model_from(model_dir);

    vector<int> horizons = {1, 3, 5, 10, 15, 30, 50, 100};
    Eigen::VectorXd assignment = Eigen::VectorXd::Constant(1, 1);
    vector<MEASURES> measures = {MEASURES::accuracy, MEASURES::f1};
    for (int horizon : horizons) {
        experimentation.compute_baseline_eval_scores_for(
            TomcatTA3::SG, horizon, measures, assignment);
        experimentation.compute_baseline_eval_scores_for(
            TomcatTA3::SY, horizon, measures, assignment);

        experimentation.compute_eval_scores_for(
            TomcatTA3::SG, horizon, measures, assignment);
        experimentation.compute_eval_scores_for(
            TomcatTA3::SY, horizon, measures, assignment);
    }

    string evaluations_dir =
        fmt::format("{}/evaluations/ta3/1b", OUTPUT_ROOT_DIR);
    experimentation.display_estimates();
    experimentation.train_and_evaluate(evaluations_dir);
}

/**
 * Generates synthetic data using the model trained in experiment 1b part a
 */
void execute_experiment_1c_part_a() {
    cout << "Experiment 1c part a\n";

    // Random Seed
    shared_ptr<gsl_rng> gen(gsl_rng_alloc(gsl_rng_mt19937));

    Experimentation experimentation(gen, Experimentation::MODEL_VERSION::v1);

    string model_dir = fmt::format("{}/model/ta3/1b", OUTPUT_ROOT_DIR);
    experimentation.load_model_from(model_dir);
    string samples_dir =
        fmt::format("{}/samples/ta3/falcon/synthetic/1c", OUTPUT_ROOT_DIR);
    experimentation.generate_synthetic_data(100, samples_dir);
}

/**
 * Performs a 5 cross validation on the synthetic data generated in part a to
 * predict victim rescuing for several values of inference horizon.
 */
void execute_experiment_1c_part_b() {
    cout << "Experiment 1c part b\n";

    // Random Seed
    shared_ptr<gsl_rng> gen(gsl_rng_alloc(gsl_rng_mt19937));

    // Data
    string data_dir = fmt::format("{}/ta3/falcon/synthetic/1c", DATA_ROOT_DIR);
    EvidenceSet data(data_dir);

    Experimentation experimentation(
        gen, "1c_cv", Experimentation::MODEL_VERSION::v1, data, 5);

    experimentation.train_using_gibbs(50, 100);
    string model_dir = fmt::format("{}/model/ta3/1c", OUTPUT_ROOT_DIR);
    experimentation.save_model(model_dir);

    vector<int> horizons = {1, 3, 5, 10, 15, 30, 50, 100};
    Eigen::VectorXd assignment = Eigen::VectorXd::Constant(1, 1);
    vector<MEASURES> measures = {MEASURES::accuracy, MEASURES::f1};
    for (int horizon : horizons) {
        experimentation.compute_baseline_eval_scores_for(
            TomcatTA3::SG, horizon, measures, assignment);
        experimentation.compute_baseline_eval_scores_for(
            TomcatTA3::SY, horizon, measures, assignment);

        experimentation.compute_eval_scores_for(
            TomcatTA3::SG, horizon, measures, assignment);
        experimentation.compute_eval_scores_for(
            TomcatTA3::SY, horizon, measures, assignment);
    }

    string evaluations_dir =
        fmt::format("{}/evaluations/ta3/1c/cv", OUTPUT_ROOT_DIR);
    experimentation.display_estimates();
    experimentation.train_and_evaluate(evaluations_dir);
}

/**
 * Evaluates the prediction of victim rescuing, for several values of inference
 * horizon, on the model trained in experiment 1b part a, using the data
 * generated in part a of the current experiment.
 */
void execute_experiment_1c_part_c() {
    cout << "Experiment 1c part c\n";

    // Random Seed
    shared_ptr<gsl_rng> gen(gsl_rng_alloc(gsl_rng_mt19937));

    // Data
    string data_dir = fmt::format("{}/ta3/falcon/synthetic/1c", DATA_ROOT_DIR);
    EvidenceSet training_set(data_dir);
    EvidenceSet test_set(data_dir);

    Experimentation experimentation(
        gen, "1c", Experimentation::MODEL_VERSION::v1, training_set, test_set);

    string model_dir = fmt::format("{}/model/ta3/1b", OUTPUT_ROOT_DIR);
    experimentation.load_model_from(model_dir);

    vector<int> horizons = {1, 3, 5, 10, 15, 30, 50, 100};
    Eigen::VectorXd assignment = Eigen::VectorXd::Constant(1, 1);
    vector<MEASURES> measures = {MEASURES::accuracy, MEASURES::f1};
    for (int horizon : horizons) {
        experimentation.compute_baseline_eval_scores_for(
            TomcatTA3::SG, horizon, measures, assignment);
        experimentation.compute_baseline_eval_scores_for(
            TomcatTA3::SY, horizon, measures, assignment);

        experimentation.compute_eval_scores_for(
            TomcatTA3::SG, horizon, measures, assignment);
        experimentation.compute_eval_scores_for(
            TomcatTA3::SY, horizon, measures, assignment);
    }

    string evaluations_dir =
        fmt::format("{}/evaluations/ta3/1c", OUTPUT_ROOT_DIR);
    experimentation.display_estimates();
    experimentation.train_and_evaluate(evaluations_dir);
}

/**
 * Generates synthetic data using the model trained in 1b part a with
 * homogeneous world up to time 6 different time steps.
 */
void execute_experiment_1d_part_a() {
    cout << "Experiment 1d part a\n";

    // Random Seed
    vector<int> time_steps = {75, 125, 200, 275, 350, 425};
    for (auto t : time_steps) {
        shared_ptr<gsl_rng> gen(gsl_rng_alloc(gsl_rng_mt19937));
        Experimentation experimentation(gen,
                                        Experimentation::MODEL_VERSION::v1);

        string model_dir = fmt::format("{}/model/ta3/1b", OUTPUT_ROOT_DIR);
        experimentation.load_model_from(model_dir);

        int max_horizon = 100;
        string samples_dir = fmt::format(
            "{}/samples/ta3/falcon/synthetic/1d/time_{}", OUTPUT_ROOT_DIR, t);
        experimentation.generate_synthetic_data(
            500, samples_dir, t, t + max_horizon);
    }
}

/**
 * Computes estimates for several values of inference horizons on the samples
 * generated in part a.
 */
void execute_experiment_1d_part_b() {
    cout << "Experiment 1d part b\n";

    vector<int> time_steps = {75, 125, 200, 275, 350, 425};
    for (auto t : time_steps) {
        // Random Seed
        shared_ptr<gsl_rng> gen(gsl_rng_alloc(gsl_rng_mt19937));

        // Data
        string data_dir =
            fmt::format("{}/ta3/falcon/synthetic/1d/time_{}", DATA_ROOT_DIR, t);
        EvidenceSet training_set(data_dir);
        EvidenceSet test_set(data_dir);
        // The samples are the same up to time t so all the predictions at this
        // time will be the same. Therefore, we only need to predict over one
        // mission trial.
        test_set.keep_first(1);

        Experimentation experimentation(gen,
                                        "1d",
                                        Experimentation::MODEL_VERSION::v1,
                                        training_set,
                                        test_set);

        string model_dir = fmt::format("{}/model/ta3/1b", OUTPUT_ROOT_DIR);
        experimentation.load_model_from(model_dir);

        vector<int> horizons = {1, 3, 5, 10, 15, 30, 50, 100};
        Eigen::VectorXd assignment = Eigen::VectorXd::Constant(1, 1);
        for (int horizon : horizons) {
            experimentation.compute_baseline_estimates_for(
                TomcatTA3::SG, horizon, assignment);
            experimentation.compute_baseline_estimates_for(
                TomcatTA3::SY, horizon, assignment);

            experimentation.compute_estimates_for(
                TomcatTA3::SG, horizon, assignment);
            experimentation.compute_estimates_for(
                TomcatTA3::SY, horizon, assignment);
        }

        string evaluations_dir =
            fmt::format("{}/evaluations/ta3/1d/time_{}", OUTPUT_ROOT_DIR, t);
        experimentation.train_and_evaluate(evaluations_dir);
    }
}

/**
 * Generates synthetic data using each one of the models trained in 1a for each
 * one of the cross validation steps.
 */
void execute_experiment_1e_part_a() {
    cout << "Experiment 1e part a\n";

    for (int fold = 1; fold <= 5; fold++) {
        // Random Seed
        shared_ptr<gsl_rng> gen(gsl_rng_alloc(gsl_rng_mt19937));

        Experimentation experimentation(gen,
                                        Experimentation::MODEL_VERSION::v1);

        string model_dir =
            fmt::format("{}/model/ta3/1a/fold{}", OUTPUT_ROOT_DIR, fold);
        experimentation.load_model_from(model_dir);

        string samples_dir = fmt::format(
            "{}/samples/ta3/falcon/synthetic/1e/fold{}", OUTPUT_ROOT_DIR, fold);
        experimentation.generate_synthetic_data(100, samples_dir);
    }
}

/**
 * Evaluates the model's performance for several values of inference horizon on
 * each one of the models trained in 1a using data generated in part a of this
 * experiment.
 */
void execute_experiment_1e_part_b() {
    cout << "Experiment 1e part b\n";

    for (int fold = 1; fold <= 5; fold++) {
        // Random Seed
        shared_ptr<gsl_rng> gen(gsl_rng_alloc(gsl_rng_mt19937));

        // Data
        string data_dir = fmt::format(
            "{}/ta3/falcon/synthetic/1e/fold{}", DATA_ROOT_DIR, fold);
        EvidenceSet training_set(data_dir);
        EvidenceSet test_set(data_dir);

        Experimentation experimentation(gen,
                                        fmt::format("1e_fold{}", fold),
                                        Experimentation::MODEL_VERSION::v1,
                                        training_set,
                                        test_set);

        string model_dir =
            fmt::format("{}/model/ta3/1a/fold{}", OUTPUT_ROOT_DIR, fold);
        experimentation.load_model_from(model_dir);

        vector<int> horizons = {1, 3, 5, 10, 15, 30, 50, 100};
        Eigen::VectorXd assignment = Eigen::VectorXd::Constant(1, 1);
        vector<MEASURES> measures = {MEASURES::accuracy, MEASURES::f1};
        for (int horizon : horizons) {
            experimentation.compute_baseline_eval_scores_for(
                TomcatTA3::SG, horizon, measures, assignment);
            experimentation.compute_baseline_eval_scores_for(
                TomcatTA3::SY, horizon, measures, assignment);

            experimentation.compute_eval_scores_for(
                TomcatTA3::SG, horizon, measures, assignment);
            experimentation.compute_eval_scores_for(
                TomcatTA3::SY, horizon, measures, assignment);
        }

        string evaluations_dir =
            fmt::format("{}/evaluations/ta3/1e/fold{}", OUTPUT_ROOT_DIR, fold);
        experimentation.display_estimates();
        experimentation.train_and_evaluate(evaluations_dir);
    }
}

void execute_experiment(const string& experiment_id) {
    if (experiment_id == "1a") {
        execute_experiment_1a();
    }
    else if (experiment_id == "1b") {
        execute_experiment_1b_part_a();
        execute_experiment_1b_part_b();
    }
    else if (experiment_id == "1b_a") {
        execute_experiment_1b_part_a();
    }
    else if (experiment_id == "1b_b") {
        execute_experiment_1b_part_b();
    }
    else if (experiment_id == "1c") {
        execute_experiment_1c_part_a();
        execute_experiment_1c_part_b();
        execute_experiment_1c_part_c();
    }
    else if (experiment_id == "1c_a") {
        execute_experiment_1c_part_a();
    }
    else if (experiment_id == "1c_b") {
        execute_experiment_1c_part_b();
    }
    else if (experiment_id == "1c_c") {
        execute_experiment_1c_part_b();
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
        "Experiment ID.\n"
        "  1a: Evaluation of victim rescuing prediction using 5-cv for several "
        "horizons on human data.\n"
        "  1b: Executes all parts of this experiment in sequence.\n"
        "  1b_a: Model training (and saving) using full human data.\n"
        "  1b_b: Evaluation of victim rescuing prediction on full human data "
        "using model trained in 1b_a.\n"
        "  1c: Executes all parts of this experiment in sequence.\n"
        "  1c_a: Synthetic data generation from the model trained in 1b_a.\n"
        "  1c_b: Evaluation of victim rescuing prediction using 5-cv for "
        "several horizons on generated data in 1c_a.\n"
        "  1c_c: Evaluation of victim rescuing prediction on data generated in "
        "1c_a using model trained in 1b_a.\n"
        "  1d: Executes all parts of this experiment in sequence.\n"
        "  1d_a: Synthetic data generation from the model trained in 1b_a with "
        "homogeneous world up to different time steps.\n"
        "  1d_b: Computes estimates using the data generated in 1c_a at the "
        "times steps used to generate the homogeneous worlds for several "
        "values of inference horizon.\n"
        "  1e: Executes all parts of this experiment in sequence.\n"
        "  1e_a: Synthetic data generation for each one of the models trained "
        "in 1a.\n"
        "  1e_b: Computes estimates for each one of the models trained in 1a "
        "using the data generated in 1e_a.\n"

    );

    po::variables_map vm;
    po::store(po::parse_command_line(argc, argv, desc), vm);
    po::notify(vm);
    if (vm.count("help") || !vm.count("experiment_id")) {
        cout << desc << "\n";
        return 1;
    }

    execute_experiment(experiment_id);
}
