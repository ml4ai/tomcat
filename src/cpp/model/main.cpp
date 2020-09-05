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
#include "pipeline/training/DBNSamplingTrainer.h"
#include "sampling/GibbsSampler.h"

using namespace tomcat::model;
using namespace std;

void execute_experiment_1a() {
    // Random Seed
    shared_ptr<gsl_rng> gen(gsl_rng_alloc(gsl_rng_mt19937));

    // Data
    EvidenceSet data("../../data/samples/ta3/falcon");
    data.set_id("falcon_engineering_data");

    // Model
    Tomcat tomcat;
    tomcat.init_ta3_learnable_model();

    // Training
    int burn_in = 10;
    int num_samples = 10;
    shared_ptr<DBNSamplingTrainer> trainer =
        make_shared<DBNSamplingTrainer>(DBNSamplingTrainer(
            gen,
            make_shared<GibbsSampler>(tomcat.get_model(), burn_in),
            num_samples));

    // Saving
    shared_ptr<DBNSaver> saver = make_shared<DBNSaver>(DBNSaver(
        tomcat.get_model(), "../../data/model/ta3/experiment_1a/fold{}"));

    // Data splitting
    int num_folds = 5;
    shared_ptr<KFold> kfold = make_shared<KFold>(gen, num_folds);

    // Estimation and evaluation
    shared_ptr<OfflineEstimation> offline_estimation =
        make_shared<OfflineEstimation>();

    shared_ptr<EvaluationAggregator> aggregator =
        make_shared<EvaluationAggregator>(
            EvaluationAggregator::METHOD::no_aggregation);

    vector<int> horizons = {1};//, 3, 5, 10, 15, 30};
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

    Pipeline pipeline;
    pipeline.set_data(data);
    pipeline.set_data_splitter(kfold);
    pipeline.set_model_trainner(trainer);
    pipeline.set_model_saver(saver);
    pipeline.set_estimation_process(offline_estimation);
    pipeline.set_aggregator(aggregator);
    pipeline.execute();

    //    MessageBrokerConfiguration config;
    //    config.timeout = 5;
    //    config.address = "localhost";
    //    config.port = 1883;
    //    shared_ptr<OnlineEstimation> online_estimation =
    //        make_shared<OnlineEstimation>(config);
    //    online_estimation->add_estimator(baseline_estimator);
}

// void test_mosquitto() {
//    MessageBrokerConfiguration config;
//    config.timeout = 5;
//    config.address = "localhost";
//    config.port = 1883;
//
//    shared_ptr<test::Mosq> mosq_ptr =
//        make_shared<test::Mosq>(test::Mosq(config));
//    mosq_ptr->init();
//}

// void test_message_passing() {
//    FactorGraph factor_graph;
//
//    Eigen::MatrixXd S0(1, 3);
//    S0 << 0.8, 0.1, 0.1;
//    factor_graph.add_node("S", 3, 0, S0, {});
//
//    Eigen::MatrixXd Q(1, 2);
//    Q << 0.6, 0.4;
//    factor_graph.add_node("Q", 2, 1, Q, {});
//
//    CPD::TableOrderingMap S1_om;
//    S1_om["S"] = {0, 3, 2};
//    S1_om["Q"] = {1, 2, 1};
//    Eigen::MatrixXd S1(6, 3);
//    S1 << 0.5, 0.4, 0.1, 0.2, 0.3, 0.5, 0.3, 0.6, 0.1, 0.6, 0.2, 0.2, 0.1,
//    0.8,
//        0.1, 0.5, 0.4, 0.1;
//    factor_graph.add_node("S", 3, 1, S1, S1_om);
//
//    CPD::TableOrderingMap G_om;
//    G_om["S"] = {0, 3, 1};
//    Eigen::MatrixXd G(3, 2);
//    G << 0.2, 0.8, 0.7, 0.3, 0.1, 0.9;
//    factor_graph.add_node("G", 2, 1, G, G_om);
//
//    CPD::TableOrderingMap Y_om;
//    Y_om["S"] = {0, 3, 1};
//    Eigen::MatrixXd Y(3, 2);
//    Y << 0.6, 0.4, 0.2, 0.8, 0.3, 0.7;
//    factor_graph.add_node("Y", 2, 1, Y, Y_om);
//
//    CPD::TableOrderingMap S2_om;
//    S2_om["S"] = {0, 3, 1};
//    Eigen::MatrixXd S2(3, 3);
//    S2 << 0.5, 0.4, 0.1, 0.2, 0.3, 0.5, 0.3, 0.6, 0.1;
//    factor_graph.add_node("S", 3, 2, S2, S2_om);
//
//    CPD::TableOrderingMap G2_om;
//    G2_om["S"] = {0, 3, 1};
//    Eigen::MatrixXd G2(3, 2);
//    G2 << 0.2, 0.8, 0.7, 0.3, 0.1, 0.9;
//    factor_graph.add_node("G", 2, 2, G2, G2_om);
//
//    CPD::TableOrderingMap Y2_om;
//    Y2_om["S"] = {0, 3, 1};
//    Eigen::MatrixXd Y2(3, 2);
//    Y2 << 0.6, 0.4, 0.2, 0.8, 0.3, 0.7;
//    factor_graph.add_node("Y", 2, 2, Y2, Y2_om);
//
//    factor_graph.add_edge("S", 0, "S", 1);
//    factor_graph.add_edge("Q", 1, "S", 1);
//    factor_graph.add_edge("S", 1, "G", 1);
//    factor_graph.add_edge("S", 1, "Y", 1);
//    factor_graph.add_edge("S", 1, "S", 2);
//    factor_graph.add_edge("S", 2, "G", 2);
//    factor_graph.add_edge("S", 2, "Y", 2);
//
//    Tensor3 tg_data = read_tensor_from_file("../../data/samples/TG.txt");
//    Tensor3 ty_data = read_tensor_from_file("../../data/samples/TY.txt");
//    EvidenceSet data;
//    data.set_id("synthetic_pipeline_testing");
//    data.add_data("G", tg_data);
//    data.add_data("Y", ty_data);
//
//    SumProductEstimator sp(nullptr, 3);
//    sp.add_node("G", Eigen::VectorXd::Constant(1, 1));
//    sp.add_node("Y", Eigen::VectorXd::Constant(1, 0));
//    sp.factor_graph = factor_graph;
//    sp.estimate(data);
//}

// DynamicBayesNet create_student_model(bool complete) {
//    // 1. PARAMETER NODES
//
//    // 1.1 GRADE
//
//    // 1.1.1 METADATA
//    vector<std::shared_ptr<NodeMetadata>> theta_g_metadatas(7);
//    for (int i = 0; i < 4; i++) {
//        stringstream parameter_label;
//        parameter_label << "Theta_G" << i;
//
//        NodeMetadata metadata =
//            NodeMetadata::create_multiple_time_link_metadata(
//                parameter_label.str(), false, true, false, 0, 3);
//        theta_g_metadatas[i] = make_shared<NodeMetadata>(move(metadata));
//    }
//
//    // 1.1.2 CPD
//    std::vector<std::shared_ptr<DirichletCPD>> theta_g_cpds(4);
//
//    Eigen::MatrixXd theta_g(1, 3);
//    theta_g << 1, 1, 1;
//    DirichletCPD theta_g_cpd_temp({}, theta_g);
//    for (int i = 0; i < 4; i++) {
//        theta_g_cpds[i] = make_shared<DirichletCPD>(theta_g_cpd_temp);
//    }
//
//    // 1.1.3 RANDOM VARIABLES
//    std::vector<std::shared_ptr<RandomVariableNode>> theta_g_nodes(4);
//    for (int i = 0; i < 4; i++) {
//        theta_g_nodes[i] =
//            make_shared<RandomVariableNode>(theta_g_metadatas[i]);
//        theta_g_nodes[i]->add_cpd_template(theta_g_cpds[i]);
//    }
//
//    // 2. VARIABLES
//
//    // 2.1 METADATAS
//    NodeMetadata grade_metadata_temp =
//        NodeMetadata::create_multiple_time_link_metadata(
//            "Grade", true, false, true, 0, 1, 3);
//    shared_ptr<NodeMetadata> grade_metadata =
//        make_shared<NodeMetadata>(move(grade_metadata_temp));
//
//    NodeMetadata difficulty_metadata_temp =
//        NodeMetadata::create_multiple_time_link_metadata(
//            "Difficulty", true, false, true, 0, 1, 2);
//    shared_ptr<NodeMetadata> difficulty_metadata =
//        make_shared<NodeMetadata>(move(difficulty_metadata_temp));
//
//    NodeMetadata intelligence_metadata_temp =
//        NodeMetadata::create_multiple_time_link_metadata(
//            "Intelligence", true, false, true, 0, 1, 2);
//    shared_ptr<NodeMetadata> intelligence_metadata =
//        make_shared<NodeMetadata>(move(intelligence_metadata_temp));
//
//    NodeMetadata sat_metadata_temp =
//        NodeMetadata::create_multiple_time_link_metadata(
//            "SAT", true, false, true, 0, 1, 2);
//    shared_ptr<NodeMetadata> sat_metadata =
//        make_shared<NodeMetadata>(move(sat_metadata_temp));
//
//    NodeMetadata letter_metadata_temp =
//        NodeMetadata::create_multiple_time_link_metadata(
//            "Letter", true, false, true, 0, 1, 2);
//    shared_ptr<NodeMetadata> letter_metadata =
//        make_shared<NodeMetadata>(move(letter_metadata_temp));
//
//    // 2.2 CPDS
//
//    // 2.2.1 Grade
//    shared_ptr<CategoricalCPD> grade_cpd;
//    if (complete) {
//        Eigen::MatrixXd grade_matrix(4, 3);
//        grade_matrix << 0.3, 0.4, 0.3, 0.05, 0.25, 0.7, 0.9, 0.08, 0.02, 0.5,
//            0.3, 0.2;
//        CategoricalCPD grade_cpd_temp(
//            {intelligence_metadata, difficulty_metadata}, grade_matrix);
//        grade_cpd = make_shared<CategoricalCPD>(std::move(grade_cpd_temp));
//    }
//    else {
//        vector<shared_ptr<Categorical>> grade_matrix;
//        grade_matrix.reserve(theta_g_nodes.size());
//        for (auto& theta_g_node : theta_g_nodes) {
//            grade_matrix.push_back(make_shared<Categorical>(theta_g_node));
//        }
//        CategoricalCPD grade_cpd_temp(
//            {intelligence_metadata, difficulty_metadata}, grade_matrix);
//        grade_cpd = make_shared<CategoricalCPD>(std::move(grade_cpd_temp));
//    }
//
//    // 2.2.2 Intelligence
//    Eigen::MatrixXd intelligence_matrix(1, 2);
//    intelligence_matrix << 0.6, 0.4;
//    CategoricalCPD intelligence_cpd_temp({}, intelligence_matrix);
//    shared_ptr<CategoricalCPD> intelligence_cpd =
//        make_shared<CategoricalCPD>(std::move(intelligence_cpd_temp));
//
//    // 2.2.3 Difficulty
//    Eigen::MatrixXd difficulty_matrix(1, 2);
//    difficulty_matrix << 0.7, 0.3;
//    CategoricalCPD difficulty_cpd_temp({}, difficulty_matrix);
//    shared_ptr<CategoricalCPD> difficulty_cpd =
//        make_shared<CategoricalCPD>(std::move(difficulty_cpd_temp));
//
//    // 2.2.4 SAT
//    Eigen::MatrixXd sat_matrix(2, 2);
//    sat_matrix << 0.95, 0.05, 0.2, 0.8;
//    CategoricalCPD sat_cpd_temp({intelligence_metadata}, sat_matrix);
//    shared_ptr<CategoricalCPD> sat_cpd =
//        make_shared<CategoricalCPD>(std::move(sat_cpd_temp));
//
//    // 2.2.4 SAT
//    Eigen::MatrixXd letter_matrix(3, 2);
//    letter_matrix << 0.1, 0.9, 0.4, 0.6, 0.99, 0.01;
//    CategoricalCPD letter_cpd_temp({grade_metadata}, letter_matrix);
//    shared_ptr<CategoricalCPD> letter_cpd =
//        make_shared<CategoricalCPD>(std::move(letter_cpd_temp));
//
//    // 2.3 RANDOM VARIABLES
//
//    std::shared_ptr<RandomVariableNode> grade(
//        make_shared<RandomVariableNode>(grade_metadata));
//    grade->add_cpd_template(grade_cpd);
//
//    std::shared_ptr<RandomVariableNode> intelligence(
//        make_shared<RandomVariableNode>(intelligence_metadata));
//    intelligence->add_cpd_template(intelligence_cpd);
//
//    std::shared_ptr<RandomVariableNode> difficulty(
//        make_shared<RandomVariableNode>(difficulty_metadata));
//    difficulty->add_cpd_template(difficulty_cpd);
//
//    std::shared_ptr<RandomVariableNode> sat(
//        make_shared<RandomVariableNode>(sat_metadata));
//    sat->add_cpd_template(sat_cpd);
//
//    std::shared_ptr<RandomVariableNode> letter(
//        make_shared<RandomVariableNode>(letter_metadata));
//    letter->add_cpd_template(letter_cpd);
//
//    // 3 CONNECTIONS
//    if (!complete) {
//        for (int i = 0; i < 4; i++) {
//            grade_metadata->add_parent_link(theta_g_metadatas[i], false);
//        }
//    }
//    grade_metadata->add_parent_link(intelligence_metadata, false);
//    grade_metadata->add_parent_link(difficulty_metadata, false);
//    letter_metadata->add_parent_link(grade_metadata, false);
//    sat_metadata->add_parent_link(intelligence_metadata, false);
//
//    // 4 DBN
//    DynamicBayesNet model;
//
//    // 4.1 ADD PARAMETER NODES
//    if (!complete) {
//        for (int i = 0; i < 4; i++) {
//            model.add_node_template(*theta_g_nodes[i]);
//        }
//    }
//
//    // 4.2 ADD VARIABLE NODES
//    model.add_node_template(*grade);
//    model.add_node_template(*intelligence);
//    model.add_node_template(*difficulty);
//    model.add_node_template(*sat);
//    model.add_node_template(*letter);
//
//    return model;
//}

int main() { execute_experiment_1a(); }
