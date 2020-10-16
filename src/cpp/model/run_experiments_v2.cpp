/**
 * This source file implements the experiments described in details in the
 * document experimentation.pdf for the version 2.0 of the ToMCAT model.
 */

#include <string>
#include <vector>

#include <boost/program_options.hpp>
#include <eigen3/Eigen/Dense>
#include <fmt/format.h>
#include <gsl/gsl_rng.h>

#include "experiments/Experimentation.h"
#include "experiments/TomcatTA3V2.h"
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
void execute_experiment_2a() {
    cout << "Experiment 2a\n";

    // Random Seed
    shared_ptr<gsl_rng> gen(gsl_rng_alloc(gsl_rng_mt19937));

    // Data
    string data_dir = fmt::format("{}/ta3/falcon/human/v2", DATA_ROOT_DIR);
    EvidenceSet data(data_dir);

    Experimentation experimentation(
        gen, "2a", Experimentation::MODEL_VERSION::v2, data, 5);

    experimentation.display_estimates();
    experimentation.train_using_gibbs(50, 100);
    string model_dir = fmt::format("{}/model/ta3/2a", OUTPUT_ROOT_DIR);
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
        fmt::format("{}/evaluations/ta3/2a", OUTPUT_ROOT_DIR);
    experimentation.train_and_evaluate(evaluations_dir);
}

/**
 * Performs a 5 cross validation on the falcon map using human data to predict
 * the training condition used in each mission trial.
 */
void execute_experiment_2b() {
    cout << "Experiment 2b\n";

    // Random Seed
    shared_ptr<gsl_rng> gen(gsl_rng_alloc(gsl_rng_mt19937));

    // Data
    string data_dir = fmt::format("{}/ta3/falcon/human/v2", DATA_ROOT_DIR);
    EvidenceSet data(data_dir);

    Experimentation experimentation(
        gen, "2a", Experimentation::MODEL_VERSION::v2, data, 5);

    experimentation.train_using_gibbs(50, 100);
    string model_dir = fmt::format("{}/model/ta3/2b", OUTPUT_ROOT_DIR);
    experimentation.save_model(model_dir);

    vector<MEASURES> measures = {MEASURES::accuracy};
    experimentation.compute_baseline_eval_scores_for(
        TomcatTA3V2::Q, 0, measures);
    experimentation.compute_eval_scores_for(TomcatTA3V2::Q, 0, measures);

    string evaluations_dir =
        fmt::format("{}/evaluations/ta3/2b", OUTPUT_ROOT_DIR);
    experimentation.display_estimates();
    experimentation.train_and_evaluate(evaluations_dir);
}

/**
 * Trains the model using the complete human data.
 */
void execute_experiment_2c_part_a() {
    cout << "Experiment 2c part a\n";

    // Random Seed
    shared_ptr<gsl_rng> gen(gsl_rng_alloc(gsl_rng_mt19937));

    // Data
    string data_dir = fmt::format("{}/ta3/falcon/human/v2", DATA_ROOT_DIR);
    EvidenceSet training_set(data_dir);
    EvidenceSet test_set;

    Experimentation experimentation(
        gen, "2c", Experimentation::MODEL_VERSION::v2, training_set, test_set);

    experimentation.train_using_gibbs(50, 100);
    string model_dir = fmt::format("{}/model/ta3/2c", OUTPUT_ROOT_DIR);
    experimentation.save_model(model_dir);
    experimentation.train_and_save();
}

/**
 * Evaluates the inference of training condition on the model trained in part a
 * using the data used to train it.
 */
void execute_experiment_2c_part_b() {
    cout << "Experiment 2c part b\n";

    // Random Seed
    shared_ptr<gsl_rng> gen(gsl_rng_alloc(gsl_rng_mt19937));

    // Data
    string data_dir = fmt::format("{}/ta3/falcon/human/v2", DATA_ROOT_DIR);
    EvidenceSet training_set(data_dir);
    EvidenceSet test_set(data_dir);

    Experimentation experimentation(
        gen, "2c", Experimentation::MODEL_VERSION::v2, training_set, test_set);

    experimentation.display_estimates();
    string model_dir = fmt::format("{}/model/ta3/2c", OUTPUT_ROOT_DIR);
    experimentation.load_model_from(model_dir);

    vector<MEASURES> measures = {MEASURES::accuracy};
    experimentation.compute_baseline_eval_scores_for(
        TomcatTA3V2::Q, 0, measures);
    experimentation.compute_eval_scores_for(TomcatTA3V2::Q, 0, measures);

    string evaluations_dir =
        fmt::format("{}/evaluations/ta3/2c", OUTPUT_ROOT_DIR);
    experimentation.train_and_evaluate(evaluations_dir);
}

/**
 * Generates synthetic data from the model trained in 2c part a.
 */
void execute_experiment_2d_part_a() {
    cout << "Experiment 2d part a\n";

    // Random Seed
    shared_ptr<gsl_rng> gen(gsl_rng_alloc(gsl_rng_mt19937));

    Experimentation experimentation(gen, Experimentation::MODEL_VERSION::v2);

    string model_dir = fmt::format("{}/model/ta3/2c", OUTPUT_ROOT_DIR);
    experimentation.load_model_from(model_dir);
    string samples_dir =
        fmt::format("{}/samples/ta3/falcon/synthetic/2d", OUTPUT_ROOT_DIR);
    experimentation.generate_synthetic_data(100, samples_dir);
}

/**
 * Performs a 5 cross validation on the falcon map using the synthetic data
 * generated in part a to predict the training condition used in each mission
 * trial.
 */
void execute_experiment_2d_part_b() {
    cout << "Experiment 2d part b\n";

    // Random Seed
    shared_ptr<gsl_rng> gen(gsl_rng_alloc(gsl_rng_mt19937));

    // Data
    string data_dir = fmt::format("{}/ta3/falcon/synthetic/2d", DATA_ROOT_DIR);
    EvidenceSet data(data_dir);

    Experimentation experimentation(
        gen, "2d_cv", Experimentation::MODEL_VERSION::v2, data, 5);

    experimentation.train_using_gibbs(50, 100);
    string model_dir = fmt::format("{}/model/ta3/2d", OUTPUT_ROOT_DIR);
    experimentation.save_model(model_dir);

    vector<MEASURES> measures = {MEASURES::accuracy};
    experimentation.compute_baseline_eval_scores_for(
        TomcatTA3V2::Q, 0, measures);
    experimentation.compute_eval_scores_for(TomcatTA3V2::Q, 0, measures);

    string evaluations_dir =
        fmt::format("{}/evaluations/ta3/2d/cv", OUTPUT_ROOT_DIR);
    experimentation.display_estimates();
    experimentation.train_and_evaluate(evaluations_dir);
}

/**
 * Evaluates the inference of training condition on the model trained in 2c part
 * a using the synthetic data generated from it in 2b part a.
 */
void execute_experiment_2d_part_c() {
    cout << "Experiment 2d part c\n";

    // Random Seed
    shared_ptr<gsl_rng> gen(gsl_rng_alloc(gsl_rng_mt19937));

    // Data
    string data_dir = fmt::format("{}/ta3/falcon/synthetic/2d", DATA_ROOT_DIR);
    EvidenceSet training_set(data_dir);
    EvidenceSet test_set(data_dir);

    Experimentation experimentation(
        gen, "2d", Experimentation::MODEL_VERSION::v2, training_set, test_set);

    string model_dir = fmt::format("{}/model/ta3/2c", OUTPUT_ROOT_DIR);
    experimentation.load_model_from(model_dir);

    vector<MEASURES> measures = {MEASURES::accuracy};
    experimentation.compute_baseline_eval_scores_for(
        TomcatTA3V2::Q, 0, measures);
    experimentation.compute_eval_scores_for(TomcatTA3V2::Q, 0, measures);

    string evaluations_dir =
        fmt::format("{}/evaluations/ta3/2d", OUTPUT_ROOT_DIR);
    experimentation.display_estimates();
    experimentation.train_and_evaluate(evaluations_dir);
}

void execute_experiment(const string& experiment_id) {
    if (experiment_id == "2a") {
        execute_experiment_2a();
    }
    else if (experiment_id == "2b") {
        execute_experiment_2b();
    }
    else if (experiment_id == "2c") {
        execute_experiment_2c_part_a();
        execute_experiment_2c_part_b();
    }
    else if (experiment_id == "2c_a") {
        execute_experiment_2c_part_a();
    }
    else if (experiment_id == "2c_b") {
        execute_experiment_2c_part_b();
    }
    else if (experiment_id == "2d") {
        execute_experiment_2d_part_a();
        execute_experiment_2d_part_b();
        execute_experiment_2d_part_c();
    }
    else if (experiment_id == "2d_a") {
        execute_experiment_2d_part_a();
    }
    else if (experiment_id == "2d_b") {
        execute_experiment_2d_part_b();
    }
    else if (experiment_id == "2d_c") {
        execute_experiment_2d_part_c();
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
        "Root directory where input data is. Human data has to be under "
        "<input_dir>/samples/ta3/human/v2.")(
        "output_dir",
        po::value<string>(&OUTPUT_ROOT_DIR)->default_value("../../data/"),
        "Output directory for generated data, model and evaluation. Generated "
        "data will be saved in "
        "<output_dir>/samples/ta3/synthetic/<experiment_id>")(
        "experiment_id",
        po::value<string>(&experiment_id)->default_value("2a"),
        "Experiment ID.\n"
        "  2a: Evaluation of victim rescuing prediction using 5-cv for several "
        "horizons on human data.\n"
        "  2b: Evaluation of training condition inference using 5-cv on human "
        "data.\n"
        "  2c: Executes all parts of this experiment in sequence.\n"
        "  2c_a: Model training (and saving) using full human data.\n"
        "  2c_b: Evaluation of training condition inference on full human data "
        "using model trained in 2c_a.\n"
        "  2d: Executes all parts of this experiment in sequence.\n"
        "  2d_a: Synthetic data generation from the model trained in 2c_a.\n"
        "  2d_b: Evaluation of training condition inference using 5-cv on data "
        "generated in 2d_a.\n"
        "  2d_c: Evaluation of training condition inference on data generated "
        "in 2d_a using model trained in 2c_a.\n");

    po::variables_map vm;
    po::store(po::parse_command_line(argc, argv, desc), vm);
    po::notify(vm);
    if (vm.count("help") || !vm.count("experiment_id")) {
        cout << desc << "\n";
        return 1;
    }

    execute_experiment(experiment_id);
}
