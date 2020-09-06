#include "Tomcat.h"

#include "pgm/EvidenceSet.h"
#include "pipeline/DBNSaver.h"
#include "pipeline/KFold.h"
#include "pipeline/Pipeline.h"
#include "pipeline/estimation/OfflineEstimation.h"
#include "pipeline/estimation/SumProductEstimator.h"
#include "pipeline/estimation/TrainingFrequencyEstimator.h"
#include "pipeline/evaluation/Accuracy.h"
#include "pipeline/evaluation/Estimates.h"
#include "pipeline/evaluation/EvaluationAggregator.h"
#include "pipeline/evaluation/F1Score.h"
#include "pipeline/training/DBNLoader.h"
#include "pipeline/training/DBNSamplingTrainer.h"
#include "sampling/GibbsSampler.h"

using namespace tomcat::model;
using namespace std;

/**
 * 5 Cross validation with the falcon map in the engineering data.
 */
void execute_experiment_1a() {
    // Random Seed
    shared_ptr<gsl_rng> gen(gsl_rng_alloc(gsl_rng_mt19937));

    // Model
    Tomcat tomcat;
    tomcat.init_ta3_learnable_model();

    // Data
    EvidenceSet data("../../data/samples/ta3/falcon/engineering");

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
        tomcat.get_model(), "../../data/model/ta3/experiment_1a/fold{}"));

    // Estimation and evaluation
    shared_ptr<OfflineEstimation> offline_estimation =
        make_shared<OfflineEstimation>();

    shared_ptr<EvaluationAggregator> aggregator =
        make_shared<EvaluationAggregator>(
            EvaluationAggregator::METHOD::no_aggregation);

    vector<int> horizons = {1, 3, 5};
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

    Pipeline pipeline("experiment_1a");
    pipeline.set_data_splitter(data_splitter);
    pipeline.set_model_trainner(trainer);
    pipeline.set_model_saver(saver);
    pipeline.set_estimation_process(offline_estimation);
    pipeline.set_aggregator(aggregator);
    pipeline.execute();
}

/**
 * Training with the engineering data and test with the human subject data
 */
void execute_experiment_1b() {
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

    Pipeline pipeline("experiment_1b");
    pipeline.set_data_splitter(data_splitter);
    pipeline.set_model_trainner(trainer);
    pipeline.set_model_saver(saver);
    pipeline.set_estimation_process(offline_estimation);
    pipeline.set_aggregator(aggregator);
    pipeline.execute();
}

void execute_experiment_1c() {
    //    MessageBrokerConfiguration config;
    //    config.timeout = 5;
    //    config.address = "localhost";
    //    config.port = 1883;
    //    shared_ptr<OnlineEstimation> online_estimation =
    //        make_shared<OnlineEstimation>(config);
    //    online_estimation->add_estimator(baseline_estimator);
}

int main() { execute_experiment_1a(); }
