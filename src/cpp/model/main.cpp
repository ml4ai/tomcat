#include "distribution/Categorical.h"
#include "distribution/Dirichlet.h"
#include "distribution/Gaussian.h"
#include "pgm/ConstantNode.h"
#include "pgm/DynamicBayesNet.h"
#include "pgm/EvidenceSet.h"
#include "pgm/Node.h"
#include "pgm/NodeMetadata.h"
#include "pgm/RandomVariableNode.h"
#include "pgm/cpd/CategoricalCPD.h"
#include "pgm/cpd/DirichletCPD.h"
#include "pgm/cpd/GaussianCPD.h"
#include "pipeline/KFold.h"
#include "pipeline/Pipeline.h"
#include "pipeline/estimation/Estimator.h"
#include "pipeline/estimation/OfflineEstimation.h"
#include "pipeline/estimation/OnlineEstimation.h"
#include "pipeline/estimation/SumProductEstimator.h"
#include "pipeline/estimation/TrainingFrequencyEstimator.h"
#include "pipeline/evaluation/Accuracy.h"
#include "pipeline/evaluation/Estimates.h"
#include "pipeline/evaluation/EvaluationAggregator.h"
#include "pipeline/evaluation/F1Score.h"
#include "pipeline/training/DBNSamplingTrainer.h"
#include "sampling/AncestralSampler.h"
#include "sampling/GibbsSampler.h"
#include "utils/Definitions.h"
#include "utils/FileHandler.h"
#include "utils/Mosquitto.h"
#include <boost/filesystem.hpp>
#include <eigen3/Eigen/Dense>
#include <fstream>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_rng.h>
#include <iostream>
#include <memory>
#include <unistd.h>
#include <variant>

namespace fs = boost::filesystem;

using namespace Eigen;
using namespace tomcat::model;
using namespace std;

class A {
  public:
    A() { cout << "New A" << endl; }
    A(A& a) { cout << "Copying A" << endl; }
    A(A&& a) { cout << "Moving A" << endl; }
    ~A() { cout << "Destroying A" << endl; }

    void print() { cout << "I am A" << endl; }
};

class B {
  private:
    A a;

  public:
    B() { cout << "New B without A" << endl; }
    B(A& a) : a(a) {
        cout << "New B with A&" << endl;
        print(a);
    }
    B(A&& a) : a(move(a)) {
        cout << "New B with A&&" << endl;
        print(a);
    }
    void print(A& a) const { a.print(); }
};

void test_shared_ptr() {
    shared_ptr<A> a = make_shared<A>(A());
    a->print();
}

void test_cpp_capabilities() {
    A a;
    B b(move(a));

    shared_ptr<A> a_ptr = make_shared<A>();
    // A a_ref = *a_ptr;
    b.print(*a_ptr);
}

void test_random_number_generation() {
    gsl_rng* gen_ptr = gsl_rng_alloc(gsl_rng_mt19937);
    gsl_rng_set(gen_ptr, time(0));
    double* probs = new double[3]{0.3, 0.4, 0.3};
    unsigned int* sample = new unsigned int[3];

    for (int i = 0; i < 20; i++) {
        int k = 7;
        double* probs = new double[k]{0, 0, 0, 0, 0, 0, 0.5};
        gsl_ran_multinomial(gen_ptr, k, 1, probs, sample);
        for (int j = 0; j < k; j++) {
            cout << sample[j] << ",";
        }
        cout << endl;
        unsigned int x = distance(sample, find(sample, sample + k, 1));
        cout << x << endl;
    }

    delete[] probs;
    delete[] sample;
    delete gen_ptr;
}

void test_dbn_entities() {
    //    cout << endl;
    //
    //    VectorXd v2 = Map<VectorXd>(theta, 2);
    //    cout << v2 << endl;
    //
    //    MatrixXd m(2, 2);
    //    m(0, 0) = 0.3;
    //    m(1, 0) = 0.8;
    //    m(0, 1) = 1 - m(0, 0);
    //    m(1, 1) = 1 - m(1, 0);
    //    cout << m << endl;
    //
    //    // Testing node metadata
    //    NodeMetadata metadata1;
    //    metadata1.label = "StateA";
    //    metadata1.initial_time_step = 0;
    //    metadata1.replicable = true;
    //    metadata1.cardinality = 2;
    //
    //    NodeMetadata metadata2;
    //    metadata2.label = "StateB";
    //    metadata2.initial_time_step = 0;
    //    metadata2.replicable = true;
    //    metadata2.cardinality = 2;
    //
    //    cout << metadata2 << endl;
    //
    //    // Testing constant nodes
    //    ConstantNode node1(2);
    //    ConstantNode node2(v, "v");
    //
    //    cout << node1 << endl;
    //    cout << node2 << endl;
    //
    //    // Testing CPDs
    //    vector<string> order{"A", "B", "C"};
    //    CategoricalCPD categorical_cpd(order, m);
    //    cout << categorical_cpd << endl;
    //    Eigen::VectorXd sample = categorical_cpd.sample(gen);
    //    cout << sample << endl;
    //
    //    GaussianCPD gaussian_cpd(order, m);
    //    cout << gaussian_cpd << endl;
    //    sample = gaussian_cpd.sample(gen);
    //    cout << sample << endl;
    //
    //    DirichletCPD dirichlet_cpd(order, m);
    //    cout << dirichlet_cpd << endl;
    //    Eigen::MatrixXd sample2 = dirichlet_cpd.sample(gen);
    //    cout << sample2 << endl;
    //
    //    // Testing RV nodes
    //    RandomVariableNode param_node1(
    //        make_shared<NodeMetadata>(metadata1),
    //        make_unique<CategoricalCPD>(categorical_cpd));
    //    cout << param_node1 << endl;
    //
    //    RandomVariableNode param_node2(
    //        make_shared<NodeMetadata>(metadata2),
    //        make_unique<CategoricalCPD>(categorical_cpd));
    //    cout << param_node2 << endl;
    //
    //    NodeMetadata metadata3 = metadata2;
    //    metadata3.label = "StateC";
    //    metadata3.add_parent_link(make_shared<RandomVariableNode>(param_node1),
    //                              true);
    //    metadata3.add_parent_link(make_shared<RandomVariableNode>(param_node2),
    //                              false);
    //
    //    RandomVariableNode data_node1(
    //        make_shared<NodeMetadata>(metadata3),
    //        make_unique<CategoricalCPD>(categorical_cpd));
    //    cout << data_node1 << endl;
    //
    //    DynamicBayesNet dbn;
    //    dbn.add_node(param_node1);
    //    dbn.add_node(param_node2);
    //    dbn.add_node(data_node1);
    // dbn.unroll(3);
}

DynamicBayesNet create_dbn(bool fixed_parameters) {
    // Creation of a simple DBN to test
    // Parameters
    NodeMetadata state_prior_metadata =
        NodeMetadata::create_single_time_link_metadata(
            "PriorS", true, false, 0, 3);

    NodeMetadata theta_s0_metadata =
        NodeMetadata::create_multiple_time_link_metadata(
            "ThetaS0", false, true, false, 1, 3);

    NodeMetadata theta_s1_metadata =
        NodeMetadata::create_multiple_time_link_metadata(
            "ThetaS1", false, true, false, 1, 3);

    NodeMetadata theta_s2_metadata =
        NodeMetadata::create_multiple_time_link_metadata(
            "ThetaS2", false, true, false, 1, 3);

    shared_ptr<NodeMetadata> state_prior_metadata_ptr =
        make_shared<NodeMetadata>(move(state_prior_metadata));

    shared_ptr<NodeMetadata> theta_s0_metadata_ptr =
        make_shared<NodeMetadata>(move(theta_s0_metadata));

    shared_ptr<NodeMetadata> theta_s1_metadata_ptr =
        make_shared<NodeMetadata>(move(theta_s1_metadata));

    shared_ptr<NodeMetadata> theta_s2_metadata_ptr =
        make_shared<NodeMetadata>(move(theta_s2_metadata));

    Eigen::MatrixXd prior_state_prior(1, 3);
    prior_state_prior << 1, 0.000001, 0.000001;
    DirichletCPD prior_state_prior_cpd({}, move(prior_state_prior));

    Eigen::MatrixXd prior_theta_s0(1, 3);
    prior_theta_s0 << 1, 1, 1;
    DirichletCPD prior_theta_s0_cpd({}, move(prior_theta_s0));

    Eigen::MatrixXd prior_theta_s1(1, 3);
    prior_theta_s1 << 1, 1, 1;
    DirichletCPD prior_theta_s1_cpd({}, move(prior_theta_s1));

    Eigen::MatrixXd prior_theta_s2(1, 3);
    prior_theta_s2 << 1, 1, 1;
    DirichletCPD prior_theta_s2_cpd({}, move(prior_theta_s2));

    shared_ptr<CPD> prior_state_prior_cpd_ptr =
        make_shared<DirichletCPD>(prior_state_prior_cpd);
    shared_ptr<CPD> prior_theta_s0_cpd_ptr =
        make_shared<DirichletCPD>(prior_theta_s0_cpd);
    shared_ptr<CPD> prior_theta_s1_cpd_ptr =
        make_shared<DirichletCPD>(prior_theta_s1_cpd);
    shared_ptr<CPD> prior_theta_s2_cpd_ptr =
        make_shared<DirichletCPD>(prior_theta_s2_cpd);

    RandomVariableNode prior_state_node(state_prior_metadata_ptr);
    prior_state_node.add_cpd_template(prior_state_prior_cpd_ptr);

    RandomVariableNode theta_s0_node(theta_s0_metadata_ptr);
    theta_s0_node.add_cpd_template(prior_theta_s0_cpd_ptr);

    RandomVariableNode theta_s1_node(theta_s1_metadata_ptr);
    theta_s1_node.add_cpd_template(prior_theta_s1_cpd_ptr);

    RandomVariableNode theta_s2_node(theta_s2_metadata_ptr);
    theta_s2_node.add_cpd_template(prior_theta_s2_cpd_ptr);

    shared_ptr<RandomVariableNode> prior_state_node_ptr =
        make_shared<RandomVariableNode>(prior_state_node);
    shared_ptr<RandomVariableNode> theta_s0_node_ptr =
        make_shared<RandomVariableNode>(theta_s0_node);
    shared_ptr<RandomVariableNode> theta_s1_node_ptr =
        make_shared<RandomVariableNode>(theta_s1_node);
    shared_ptr<RandomVariableNode> theta_s2_node_ptr =
        make_shared<RandomVariableNode>(theta_s2_node);

    NodeMetadata state_metadata =
        NodeMetadata::create_multiple_time_link_metadata(
            "State", true, false, true, 0, 1, 3);

    shared_ptr<NodeMetadata> state_metadata_ptr =
        make_shared<NodeMetadata>(state_metadata);

    CategoricalCPD prior_state_cpd(
        {}, {make_shared<Categorical>(Categorical(prior_state_node_ptr))});

    shared_ptr<CPD> prior_state_cpd_ptr =
        make_shared<CategoricalCPD>(prior_state_cpd);

    Eigen::MatrixXd state_transition_matrix(3, 3);
    state_transition_matrix << 0, 0, 1, 1, 0, 0, 0, 1, 0;
    CategoricalCPD state_cpd({state_metadata_ptr}, state_transition_matrix);
    if (!fixed_parameters) {
        state_cpd =
            CategoricalCPD({state_metadata_ptr},
                           {make_shared<Categorical>(theta_s0_node_ptr),
                            make_shared<Categorical>(theta_s1_node_ptr),
                            make_shared<Categorical>(theta_s2_node_ptr)});
    }

    shared_ptr<CPD> state_cpd_ptr = make_shared<CategoricalCPD>(state_cpd);

    RandomVariableNode state_node(state_metadata_ptr);
    state_node.add_cpd_template(prior_state_cpd_ptr);
    state_node.add_cpd_template(state_cpd_ptr);

    shared_ptr<RandomVariableNode> state_node_ptr =
        make_shared<RandomVariableNode>(state_node);
    state_metadata_ptr->add_parent_link(state_metadata_ptr, true);
    state_metadata_ptr->add_parent_link(state_prior_metadata_ptr, false);
    if (!fixed_parameters) {
        state_metadata_ptr->add_parent_link(theta_s0_metadata_ptr, true);
        state_metadata_ptr->add_parent_link(theta_s1_metadata_ptr, true);
        state_metadata_ptr->add_parent_link(theta_s2_metadata_ptr, true);
    }

    NodeMetadata tg_metadata = NodeMetadata::create_multiple_time_link_metadata(
        "TG", true, false, true, 1, 1, 2);
    tg_metadata.add_parent_link(state_metadata_ptr, false);

    Eigen::MatrixXd tg_emission_matrix(3, 2);
    tg_emission_matrix << 1, 0, 0, 1, 0.5, 0.5;
    CategoricalCPD tg_cpd({state_metadata_ptr}, move(tg_emission_matrix));

    RandomVariableNode tg_node(make_shared<NodeMetadata>(tg_metadata));
    tg_node.add_cpd_template(make_shared<CategoricalCPD>(tg_cpd));

    NodeMetadata ty_metadata = NodeMetadata::create_multiple_time_link_metadata(
        "TY", true, false, true, 1, 1, 2);
    ty_metadata.add_parent_link(state_metadata_ptr, false);

    Eigen::MatrixXd ty_emission_matrix(3, 2);
    ty_emission_matrix << 0, 1, 1, 0, 0.5, 0.5;
    CategoricalCPD ty_cpd({state_metadata_ptr}, move(ty_emission_matrix));

    RandomVariableNode ty_node(make_shared<NodeMetadata>(ty_metadata));
    ty_node.add_cpd_template(make_shared<CategoricalCPD>(ty_cpd));

    DynamicBayesNet dbn(3);
    dbn.add_node_template(state_node);
    dbn.add_node_template(tg_node);
    dbn.add_node_template(ty_node);
    dbn.add_node_template(prior_state_node);
    if (!fixed_parameters) {
        dbn.add_node_template(move(theta_s0_node));
        dbn.add_node_template(move(theta_s1_node));
        dbn.add_node_template(move(theta_s2_node));
    }

    return dbn;
}

void generate_samples_to_test(shared_ptr<DynamicBayesNet> dbn,
                              int num_samples,
                              shared_ptr<gsl_rng> gen) {
    AncestralSampler sampler(dbn);
    sampler.sample(gen, num_samples);
    sampler.save_samples_to_folder("../../data/samples");
}

void sample_parameters_from_posterior(shared_ptr<DynamicBayesNet> dbn,
                                      int num_samples,
                                      shared_ptr<gsl_rng> gen) {
    Tensor3 tg_data = read_tensor_from_file("../../data/samples/TG.txt");
    Tensor3 ty_data = read_tensor_from_file("../../data/samples/TY.txt");
    EvidenceSet data;
    data.add_data("TG", tg_data);
    data.add_data("TY", ty_data);
    GibbsSampler gibbs(dbn, 20);
    gibbs.set_num_in_plate_samples(data.get_num_data_points());
    gibbs.add_data(data);
    gibbs.sample(gen, num_samples);

    cout << "ThetaS0" << endl;
    cout << gibbs.get_samples("ThetaS0") << endl;

    cout << "ThetaS1" << endl;
    cout << gibbs.get_samples("ThetaS1") << endl;

    cout << "ThetaS2" << endl;
    cout << gibbs.get_samples("ThetaS2") << endl;
}

void train_dbn(shared_ptr<DynamicBayesNet> dbn,
               int num_samples,
               shared_ptr<gsl_rng> gen) {

    Tensor3 tg_data = read_tensor_from_file("../../data/samples/TG.txt");
    Tensor3 ty_data = read_tensor_from_file("../../data/samples/TY.txt");
    EvidenceSet data;
    data.add_data("TG", tg_data);
    data.add_data("TY", ty_data);
    GibbsSampler gibbs(dbn, 20);
    gibbs.set_num_in_plate_samples(data.get_num_data_points());

    DBNSamplingTrainer trainer(
        gen, make_shared<GibbsSampler>(gibbs), num_samples);
    trainer.fit(data);

    dbn->save_to_folder("../../data/model");
}

void test_baseline_estimator(shared_ptr<DynamicBayesNet> model) {
    shared_ptr<TrainingFrequencyEstimator> estimator =
        make_shared<TrainingFrequencyEstimator>(
            TrainingFrequencyEstimator(model, 1));
    estimator->estimate(EvidenceSet());
}

void test_sum_product_estimator(shared_ptr<DynamicBayesNet> model) {
    shared_ptr<SumProductEstimator> estimator =
        make_shared<SumProductEstimator>(SumProductEstimator(model, 2));
    estimator->estimate(EvidenceSet());
}

void test_pipeline() {
    Tensor3 tg_data = read_tensor_from_file("../../data/samples/TG.txt");
    Tensor3 ty_data = read_tensor_from_file("../../data/samples/TY.txt");
    EvidenceSet data;
    data.set_id("synthetic_pipeline_testing");
    data.add_data("TG", tg_data);
    data.add_data("TY", ty_data);

    shared_ptr<gsl_rng> gen(gsl_rng_alloc(gsl_rng_mt19937));

    shared_ptr<DynamicBayesNet> model =
        make_shared<DynamicBayesNet>(create_dbn(false));
    model->unroll(20, true);

    GibbsSampler gibbs(model, 100);

    shared_ptr<DBNSamplingTrainer> trainer = make_shared<DBNSamplingTrainer>(
        DBNSamplingTrainer(gen, make_shared<GibbsSampler>(gibbs), 5));

    shared_ptr<DBNSaver> saver = make_shared<DBNSaver>(
        DBNSaver(model, "../../data/model/pipeline_test/fold{}"));

    shared_ptr<KFold> kfold = make_shared<KFold>(gen, 2);

    shared_ptr<TrainingFrequencyEstimator> baseline_estimator =
        make_shared<TrainingFrequencyEstimator>(
            TrainingFrequencyEstimator(model, 1));
    baseline_estimator->add_node("TG", Eigen::VectorXd::Constant(1, 1));
    baseline_estimator->add_node("TY", Eigen::VectorXd::Constant(1, 1));
    shared_ptr<SumProductEstimator> sumproduct_estimator =
        make_shared<SumProductEstimator>(SumProductEstimator(model, 1));
    sumproduct_estimator->add_node("TG", Eigen::VectorXd::Constant(1, 1));
    sumproduct_estimator->add_node("TY", Eigen::VectorXd::Constant(1, 1));

    MessageBrokerConfiguration config;
    config.timeout = 5;
    config.address = "localhost";
    config.port = 1883;
    shared_ptr<OnlineEstimation> online_estimation =
        make_shared<OnlineEstimation>(config);
    online_estimation->add_estimator(baseline_estimator);
    shared_ptr<OfflineEstimation> offline_estimation =
        make_shared<OfflineEstimation>();
    offline_estimation->add_estimator(baseline_estimator);
    offline_estimation->add_estimator(sumproduct_estimator);

    shared_ptr<EvaluationAggregator> aggregator =
        make_shared<EvaluationAggregator>(
            EvaluationAggregator::METHOD::no_aggregation);

    shared_ptr<Estimates> estimates =
        make_shared<Estimates>(baseline_estimator);
    shared_ptr<Accuracy> accuracy = make_shared<Accuracy>(baseline_estimator);
    shared_ptr<F1Score> f1_score = make_shared<F1Score>(baseline_estimator);


    shared_ptr<Estimates> estimates_sp =
        make_shared<Estimates>(sumproduct_estimator);
    shared_ptr<Accuracy> accuracy_sp =
        make_shared<Accuracy>(sumproduct_estimator);

//    aggregator->add_measure(estimates);
//    aggregator->add_measure(accuracy);
//    aggregator->add_measure(f1_score);
    aggregator->add_measure(estimates_sp);
    aggregator->add_measure(accuracy_sp);


    Pipeline pipeline;
    pipeline.set_data(data);
    pipeline.set_data_splitter(kfold);
    pipeline.set_model_trainner(trainer);
    pipeline.set_model_saver(saver);
    //pipeline.set_estimation_process(online_estimation);
    pipeline.set_estimation_process(offline_estimation);
    pipeline.set_aggregator(aggregator);

    pipeline.execute();
}
namespace test {
    class Test {
      public:
        virtual void init() = 0;

        bool yay;
    };

    class Mosq : public Test, public Mosquitto {
      public:
        Mosq(MessageBrokerConfiguration config) : config(config) {}

        ~Mosq() {}

        void init() override {
            this->connect(config.address, config.port, 60);
            this->subscribe("toy/test");
            this->subscribe("toy/test2");
            this->loop(true);
            this->close();
        }

        void on_message(const string& topic, const string& message) override {}

        void on_error(const string& error_message) override {}

      private:
        MessageBrokerConfiguration config;
    };
} // namespace test

void test_mosquitto() {
    MessageBrokerConfiguration config;
    config.timeout = 5;
    config.address = "localhost";
    config.port = 1883;

    shared_ptr<test::Mosq> mosq_ptr =
        make_shared<test::Mosq>(test::Mosq(config));
    mosq_ptr->init();
}

void test_message_passing() {
    FactorGraph factor_graph;

    Eigen::MatrixXd S0(1, 3);
    S0 << 0.8, 0.1, 0.1;
    factor_graph.add_node("S", 3, 0, S0, {});

    Eigen::MatrixXd Q(1, 2);
    Q << 0.6, 0.4;
    factor_graph.add_node("Q", 2, 1, Q, {});

    CPD::TableOrderingMap S1_om;
    S1_om["S"] = {0, 3, 2};
    S1_om["Q"] = {1, 2, 1};
    Eigen::MatrixXd S1(6, 3);
    S1 << 0.5, 0.4, 0.1, 0.2, 0.3, 0.5, 0.3, 0.6, 0.1, 0.6, 0.2, 0.2, 0.1, 0.8,
        0.1, 0.5, 0.4, 0.1;
    factor_graph.add_node("S", 3, 1, S1, S1_om);

    CPD::TableOrderingMap G_om;
    G_om["S"] = {0, 3, 1};
    Eigen::MatrixXd G(3, 2);
    G << 0.2, 0.8, 0.7, 0.3, 0.1, 0.9;
    factor_graph.add_node("G", 2, 1, G, G_om);

    CPD::TableOrderingMap Y_om;
    Y_om["S"] = {0, 3, 1};
    Eigen::MatrixXd Y(3, 2);
    Y << 0.6, 0.4, 0.2, 0.8, 0.3, 0.7;
    factor_graph.add_node("Y", 2, 1, Y, Y_om);

    CPD::TableOrderingMap S2_om;
    S2_om["S"] = {0, 3, 1};
    Eigen::MatrixXd S2(3, 3);
    S2 << 0.5, 0.4, 0.1, 0.2, 0.3, 0.5, 0.3, 0.6, 0.1;
    factor_graph.add_node("S", 3, 2, S2, S2_om);

    CPD::TableOrderingMap G2_om;
    G2_om["S"] = {0, 3, 1};
    Eigen::MatrixXd G2(3, 2);
    G2 << 0.2, 0.8, 0.7, 0.3, 0.1, 0.9;
    factor_graph.add_node("G", 2, 2, G2, G2_om);

    CPD::TableOrderingMap Y2_om;
    Y2_om["S"] = {0, 3, 1};
    Eigen::MatrixXd Y2(3, 2);
    Y2 << 0.6, 0.4, 0.2, 0.8, 0.3, 0.7;
    factor_graph.add_node("Y", 2, 2, Y2, Y2_om);

    factor_graph.add_edge("S", 0, "S", 1);
    factor_graph.add_edge("Q", 1, "S", 1);
    factor_graph.add_edge("S", 1, "G", 1);
    factor_graph.add_edge("S", 1, "Y", 1);
    factor_graph.add_edge("S", 1, "S", 2);
    factor_graph.add_edge("S", 2, "G", 2);
    factor_graph.add_edge("S", 2, "Y", 2);

    Tensor3 tg_data = read_tensor_from_file("../../data/samples/TG.txt");
    Tensor3 ty_data = read_tensor_from_file("../../data/samples/TY.txt");
    EvidenceSet data;
    data.set_id("synthetic_pipeline_testing");
    data.add_data("G", tg_data);
    data.add_data("Y", ty_data);

    SumProductEstimator sp(nullptr, 3);
    sp.add_node("G", Eigen::VectorXd::Constant(1, 1));
    sp.add_node("Y", Eigen::VectorXd::Constant(1, 0));
    sp.factor_graph = factor_graph;
    sp.estimate(data);
}

DynamicBayesNet create_student_model(bool complete) {
    // 1. PARAMETER NODES

    // 1.1 GRADE

    // 1.1.1 METADATA
    vector<std::shared_ptr<NodeMetadata>> theta_g_metadatas(7);
    for (int i = 0; i < 4; i++) {
        stringstream parameter_label;
        parameter_label << "Theta_G" << i;

        NodeMetadata metadata =
            NodeMetadata::create_multiple_time_link_metadata(
                parameter_label.str(), false, true, false, 0, 3);
        theta_g_metadatas[i] = make_shared<NodeMetadata>(move(metadata));
    }

    // 1.1.2 CPD
    std::vector<std::shared_ptr<DirichletCPD>> theta_g_cpds(4);

    Eigen::MatrixXd theta_g(1, 3);
    theta_g << 1, 1, 1;
    DirichletCPD theta_g_cpd_temp({}, theta_g);
    for (int i = 0; i < 4; i++) {
        theta_g_cpds[i] = make_shared<DirichletCPD>(theta_g_cpd_temp);
    }

    // 1.1.3 RANDOM VARIABLES
    std::vector<std::shared_ptr<RandomVariableNode>> theta_g_nodes(4);
    for (int i = 0; i < 4; i++) {
        theta_g_nodes[i] =
            make_shared<RandomVariableNode>(theta_g_metadatas[i]);
        theta_g_nodes[i]->add_cpd_template(theta_g_cpds[i]);
    }

    // 2. VARIABLES

    // 2.1 METADATAS
    NodeMetadata grade_metadata_temp =
        NodeMetadata::create_multiple_time_link_metadata(
            "Grade", true, false, true, 0, 1, 3);
    shared_ptr<NodeMetadata> grade_metadata =
        make_shared<NodeMetadata>(move(grade_metadata_temp));

    NodeMetadata difficulty_metadata_temp =
        NodeMetadata::create_multiple_time_link_metadata(
            "Difficulty", true, false, true, 0, 1, 2);
    shared_ptr<NodeMetadata> difficulty_metadata =
        make_shared<NodeMetadata>(move(difficulty_metadata_temp));

    NodeMetadata intelligence_metadata_temp =
        NodeMetadata::create_multiple_time_link_metadata(
            "Intelligence", true, false, true, 0, 1, 2);
    shared_ptr<NodeMetadata> intelligence_metadata =
        make_shared<NodeMetadata>(move(intelligence_metadata_temp));

    NodeMetadata sat_metadata_temp =
        NodeMetadata::create_multiple_time_link_metadata(
            "SAT", true, false, true, 0, 1, 2);
    shared_ptr<NodeMetadata> sat_metadata =
        make_shared<NodeMetadata>(move(sat_metadata_temp));

    NodeMetadata letter_metadata_temp =
        NodeMetadata::create_multiple_time_link_metadata(
            "Letter", true, false, true, 0, 1, 2);
    shared_ptr<NodeMetadata> letter_metadata =
        make_shared<NodeMetadata>(move(letter_metadata_temp));

    // 2.2 CPDS

    // 2.2.1 Grade
    shared_ptr<CategoricalCPD> grade_cpd;
    if (complete) {
        Eigen::MatrixXd grade_matrix(4, 3);
        grade_matrix << 0.3, 0.4, 0.3, 0.05, 0.25, 0.7, 0.9, 0.08, 0.02, 0.5,
            0.3, 0.2;
        CategoricalCPD grade_cpd_temp(
            {intelligence_metadata, difficulty_metadata}, grade_matrix);
        grade_cpd = make_shared<CategoricalCPD>(std::move(grade_cpd_temp));
    }
    else {
        vector<shared_ptr<Categorical>> grade_matrix;
        grade_matrix.reserve(theta_g_nodes.size());
        for (auto& theta_g_node : theta_g_nodes) {
            grade_matrix.push_back(make_shared<Categorical>(theta_g_node));
        }
        CategoricalCPD grade_cpd_temp(
            {intelligence_metadata, difficulty_metadata}, grade_matrix);
        grade_cpd = make_shared<CategoricalCPD>(std::move(grade_cpd_temp));
    }

    // 2.2.2 Intelligence
    Eigen::MatrixXd intelligence_matrix(1, 2);
    intelligence_matrix << 0.6, 0.4;
    CategoricalCPD intelligence_cpd_temp({}, intelligence_matrix);
    shared_ptr<CategoricalCPD> intelligence_cpd =
        make_shared<CategoricalCPD>(std::move(intelligence_cpd_temp));

    // 2.2.3 Difficulty
    Eigen::MatrixXd difficulty_matrix(1, 2);
    difficulty_matrix << 0.7, 0.3;
    CategoricalCPD difficulty_cpd_temp({}, difficulty_matrix);
    shared_ptr<CategoricalCPD> difficulty_cpd =
        make_shared<CategoricalCPD>(std::move(difficulty_cpd_temp));

    // 2.2.4 SAT
    Eigen::MatrixXd sat_matrix(2, 2);
    sat_matrix << 0.95, 0.05, 0.2, 0.8;
    CategoricalCPD sat_cpd_temp({intelligence_metadata}, sat_matrix);
    shared_ptr<CategoricalCPD> sat_cpd =
        make_shared<CategoricalCPD>(std::move(sat_cpd_temp));

    // 2.2.4 SAT
    Eigen::MatrixXd letter_matrix(3, 2);
    letter_matrix << 0.1, 0.9, 0.4, 0.6, 0.99, 0.01;
    CategoricalCPD letter_cpd_temp({grade_metadata}, letter_matrix);
    shared_ptr<CategoricalCPD> letter_cpd =
        make_shared<CategoricalCPD>(std::move(letter_cpd_temp));

    // 2.3 RANDOM VARIABLES

    std::shared_ptr<RandomVariableNode> grade(
        make_shared<RandomVariableNode>(grade_metadata));
    grade->add_cpd_template(grade_cpd);

    std::shared_ptr<RandomVariableNode> intelligence(
        make_shared<RandomVariableNode>(intelligence_metadata));
    intelligence->add_cpd_template(intelligence_cpd);

    std::shared_ptr<RandomVariableNode> difficulty(
        make_shared<RandomVariableNode>(difficulty_metadata));
    difficulty->add_cpd_template(difficulty_cpd);

    std::shared_ptr<RandomVariableNode> sat(
        make_shared<RandomVariableNode>(sat_metadata));
    sat->add_cpd_template(sat_cpd);

    std::shared_ptr<RandomVariableNode> letter(
        make_shared<RandomVariableNode>(letter_metadata));
    letter->add_cpd_template(letter_cpd);

    // 3 CONNECTIONS
    if(!complete) {
        for (int i = 0; i < 4; i++) {
            grade_metadata->add_parent_link(theta_g_metadatas[i], false);
        }
    }
    grade_metadata->add_parent_link(intelligence_metadata, false);
    grade_metadata->add_parent_link(difficulty_metadata, false);
    letter_metadata->add_parent_link(grade_metadata, false);
    sat_metadata->add_parent_link(intelligence_metadata, false);

    // 4 DBN
    DynamicBayesNet model;

    // 4.1 ADD PARAMETER NODES
    if(!complete) {
        for (int i = 0; i < 4; i++) {
            model.add_node_template(*theta_g_nodes[i]);
        }
    }

    // 4.2 ADD VARIABLE NODES
    model.add_node_template(*grade);
    model.add_node_template(*intelligence);
    model.add_node_template(*difficulty);
    model.add_node_template(*sat);
    model.add_node_template(*letter);

    return model;
}

void generate_student_data() {
    shared_ptr<DynamicBayesNet> model =
        make_shared<DynamicBayesNet>(create_student_model(true));
    shared_ptr<gsl_rng> gen(gsl_rng_alloc(gsl_rng_mt19937));
    model->unroll(1, true);
    AncestralSampler sampler(model);
    sampler.set_num_in_plate_samples(1);
    sampler.sample(gen, 500);
    sampler.save_samples_to_folder("../../data/samples/student");
}

void test_student_training_runtime() {
    shared_ptr<DynamicBayesNet> model =
        make_shared<DynamicBayesNet>(create_student_model(false));

    Tensor3 difficulty_data = read_tensor_from_file("../../data/samples/student/Difficulty.txt");
    Tensor3 intelligence_data =
        read_tensor_from_file("../../data/samples/student/Intelligence.txt");
    Tensor3 sat_data = read_tensor_from_file(
        "../../data/samples/student/SAT.txt");
    Tensor3 letter_data = read_tensor_from_file(
        "../../data/samples/student/Letter.txt");
    EvidenceSet data;
    data.add_data("Difficulty", difficulty_data);
    data.add_data("Intelligence", intelligence_data);
    data.add_data("SAT", sat_data);
    data.add_data("Letter", letter_data);

    model->unroll(data.get_time_steps(), true);
    shared_ptr<GibbsSampler> gibbs = make_shared<GibbsSampler>(model, 500);
    gibbs->set_num_in_plate_samples(data.get_num_data_points());

    Timer timer;
    shared_ptr<gsl_rng> gen(gsl_rng_alloc(gsl_rng_mt19937));
    DBNSamplingTrainer trainer(gen, gibbs, 5000);
    trainer.fit(data);

    shared_ptr<DBNSaver> saver =
        make_shared<DBNSaver>(model, "../../data/model/student");
    saver->save();
}

DynamicBayesNet create_tomcat_model() {
    // 1. PARAMETER NODES

    // 1.1 STATES

    // 1.1.1 METADATA
    vector<std::shared_ptr<NodeMetadata>> theta_s_metadatas(7);
    for (int i = 0; i < 7; i++) {
        stringstream parameter_label;
        parameter_label << "Theta_S" << i;

        NodeMetadata metadata =
            NodeMetadata::create_multiple_time_link_metadata(
                parameter_label.str(), false, true, false, 1, 7);
        theta_s_metadatas[i] = make_shared<NodeMetadata>(move(metadata));
    }

    // 1.1.2 CPD
    std::vector<std::shared_ptr<DirichletCPD>> theta_s_cpds(7);

    // From HW to HW | LRW | DRW
    Eigen::MatrixXd theta_s0(1, 7);
    theta_s0 << 1, 1, EPSILON, EPSILON, 1, EPSILON, EPSILON;
    DirichletCPD theta_s_cpd_temp({}, theta_s0);
    theta_s_cpds[0] = make_shared<DirichletCPD>(move(theta_s_cpd_temp));

    // From LRW to HW | LRW | LTG | LTY | DRW
    Eigen::MatrixXd theta_s1(1, 7);
    theta_s1 << 1, 1, 1, 1, 1, EPSILON, EPSILON;
    theta_s_cpd_temp = DirichletCPD({}, theta_s1);
    theta_s_cpds[1] = make_shared<DirichletCPD>(move(theta_s_cpd_temp));

    // From LTG to LRW | LTG
    Eigen::MatrixXd theta_s2(1, 7);
    theta_s2 << EPSILON, 1, 1, EPSILON, EPSILON, EPSILON, EPSILON;
    theta_s_cpd_temp = DirichletCPD({}, theta_s2);
    theta_s_cpds[2] = make_shared<DirichletCPD>(move(theta_s_cpd_temp));

    // From LTY to LRW | LTY
    Eigen::MatrixXd theta_s3(1, 7);
    theta_s3 << EPSILON, 1, EPSILON, 1, EPSILON, EPSILON, EPSILON;
    theta_s_cpd_temp = DirichletCPD({}, theta_s3);
    theta_s_cpds[3] = make_shared<DirichletCPD>(move(theta_s_cpd_temp));

    // From DRW to HW | LRW | DRW | DTG | DTY
    Eigen::MatrixXd theta_s4(1, 7);
    theta_s4 << 1, 1, EPSILON, EPSILON, 1, 1, 1;
    theta_s_cpd_temp = DirichletCPD({}, theta_s4);
    theta_s_cpds[4] = make_shared<DirichletCPD>(move(theta_s_cpd_temp));

    // From DTG to DRW | DTG
    Eigen::MatrixXd theta_s5(1, 7);
    theta_s5 << EPSILON, EPSILON, EPSILON, EPSILON, 1, 1, EPSILON;
    theta_s_cpd_temp = DirichletCPD({}, theta_s5);
    theta_s_cpds[5] = make_shared<DirichletCPD>(move(theta_s_cpd_temp));

    // From DTY to DRW | DTY
    Eigen::MatrixXd theta_s6(1, 7);
    theta_s6 << EPSILON, EPSILON, EPSILON, EPSILON, 1, EPSILON, 1;
    theta_s_cpd_temp = DirichletCPD({}, theta_s6);
    theta_s_cpds[6] = make_shared<DirichletCPD>(move(theta_s_cpd_temp));

    // 1.1.3 RANDOM VARIABLES
    std::vector<std::shared_ptr<RandomVariableNode>> theta_s_nodes(7);
    for (int i = 0; i < 7; i++) {
        theta_s_nodes[i] =
            make_shared<RandomVariableNode>(theta_s_metadatas[i]);
        theta_s_nodes[i]->add_cpd_template(theta_s_cpds[i]);
    }

    // 1.2 LIGHTS

    // 1.2.1 METADATA
    vector<std::shared_ptr<NodeMetadata>> pi_lt_metadatas(7);
    for (int i = 0; i < 7; i++) {
        stringstream parameter_label;
        parameter_label << "Pi_LT" << i;

        NodeMetadata metadata =
            NodeMetadata::create_multiple_time_link_metadata(
                parameter_label.str(), false, true, false, 1, 2);
        pi_lt_metadatas[i] = make_shared<NodeMetadata>(move(metadata));
    }

    // 1.2.2 CPDS
    std::vector<std::shared_ptr<DirichletCPD>> pi_lt_cpds(7);

    Eigen::MatrixXd pi_lt0(1, 2);
    pi_lt0 << 1, 1;
    DirichletCPD pi_lt_cpd_temp({}, pi_lt0);
    pi_lt_cpds[0] = make_shared<DirichletCPD>(move(pi_lt_cpd_temp));

    // States where the light is always on
    Eigen::MatrixXd pi_lt_on(1, 2);
    pi_lt_on << EPSILON, 1;
    pi_lt_cpd_temp = DirichletCPD({}, pi_lt_on);
    for (int i = 1; i < 4; i++) {
        pi_lt_cpds[i] = make_shared<DirichletCPD>(pi_lt_cpd_temp);
    }

    // States where the light is always off
    Eigen::MatrixXd pi_lt_off(1, 2);
    pi_lt_off << 1, EPSILON;
    pi_lt_cpd_temp = DirichletCPD({}, pi_lt_off);
    for (int i = 4; i < 7; i++) {
        pi_lt_cpds[i] = make_shared<DirichletCPD>(pi_lt_cpd_temp);
    }

    // 1.2.3 RANDOM VARIABLES
    std::vector<std::shared_ptr<RandomVariableNode>> pi_lt_nodes(7);
    for (int i = 0; i < 7; i++) {
        pi_lt_nodes[i] = make_shared<RandomVariableNode>(pi_lt_metadatas[i]);
        pi_lt_nodes[i]->add_cpd_template(pi_lt_cpds[i]);
    }

    // 2. VARIABLES

    // 2.1 METADATAS
    NodeMetadata state_metadata_temp =
        NodeMetadata::create_multiple_time_link_metadata(
            "State", true, false, true, 0, 1, 7);
    shared_ptr<NodeMetadata> state_metadata =
        make_shared<NodeMetadata>(move(state_metadata_temp));

    NodeMetadata light_metadata_temp =
        NodeMetadata::create_multiple_time_link_metadata(
            "Light", true, false, true, 1, 1, 2);
    shared_ptr<NodeMetadata> light_metadata =
        make_shared<NodeMetadata>(move(light_metadata_temp));

    NodeMetadata room_metadata_temp =
        NodeMetadata::create_multiple_time_link_metadata(
            "Room", true, false, true, 1, 1, 2);
    shared_ptr<NodeMetadata> room_metadata =
        make_shared<NodeMetadata>(move(room_metadata_temp));

    NodeMetadata tg_metadata_temp =
        NodeMetadata::create_multiple_time_link_metadata(
            "TG", true, false, true, 1, 1, 2);
    shared_ptr<NodeMetadata> tg_metadata =
        make_shared<NodeMetadata>(move(tg_metadata_temp));

    NodeMetadata ty_metadata_temp =
        NodeMetadata::create_multiple_time_link_metadata(
            "TY", true, false, true, 1, 1, 2);
    shared_ptr<NodeMetadata> ty_metadata =
        make_shared<NodeMetadata>(move(ty_metadata_temp));

    // 2.2 CPDS

    // 2.2.1 State Prior
    Eigen::MatrixXd state_prior = Eigen::MatrixXd::Constant(1, 7, EPSILON);
    state_prior(0, 0) = 1; // The first state is always 0
    CategoricalCPD state_prior_cpd_temp({}, state_prior);
    shared_ptr<CategoricalCPD> state_prior_cpd =
        make_shared<CategoricalCPD>(move(state_prior_cpd_temp));

    // 2.2.2 State Transition
    vector<shared_ptr<Categorical>> state_transition_matrix;
    state_transition_matrix.reserve(theta_s_nodes.size());
    for (auto& theta_s_node : theta_s_nodes) {
        state_transition_matrix.push_back(
            make_shared<Categorical>(theta_s_node));
    }
    CategoricalCPD state_transition_cpd_temp({state_metadata},
                                             state_transition_matrix);
    shared_ptr<CategoricalCPD> state_transition_cpd =
        make_shared<CategoricalCPD>(std::move(state_transition_cpd_temp));

    // 2.2.2 Light Emission
    vector<shared_ptr<Categorical>> light_emission_matrix;
    light_emission_matrix.reserve(pi_lt_nodes.size());
    for (auto& pi_lt_node : pi_lt_nodes) {
        light_emission_matrix.push_back(make_shared<Categorical>(pi_lt_node));
    }
    CategoricalCPD light_emission_cpd_temp({state_metadata},
                                           light_emission_matrix);
    shared_ptr<CategoricalCPD> light_emission_cpd =
        make_shared<CategoricalCPD>(std::move(light_emission_cpd_temp));

    // 2.2.2 Room Emission

    // HW (the first state) is the only state where the player is not in a room
    // but in the hallway
    Eigen::MatrixXd room_emission_matrix(7, 2);
    room_emission_matrix << 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0;
    CategoricalCPD room_emission_cpd_temp({state_metadata},
                                          room_emission_matrix);
    shared_ptr<CategoricalCPD> room_emission_cpd =
        make_shared<CategoricalCPD>(std::move(room_emission_cpd_temp));

    // 2.2.3 TG Emission

    // HW (the first state) is the only state where the player is not in a room
    // but in the hallway
    Eigen::MatrixXd tg_emission_matrix(7, 2);
    tg_emission_matrix << 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0;
    CategoricalCPD tg_emission_cpd_temp({state_metadata}, tg_emission_matrix);
    shared_ptr<CategoricalCPD> tg_emission_cpd =
        make_shared<CategoricalCPD>(std::move(tg_emission_cpd_temp));

    Eigen::MatrixXd ty_emission_matrix(7, 2);
    ty_emission_matrix << 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0;
    CategoricalCPD ty_emission_cpd_temp({state_metadata}, ty_emission_matrix);
    shared_ptr<CategoricalCPD> ty_emission_cpd =
        make_shared<CategoricalCPD>(std::move(ty_emission_cpd_temp));

    // 2.3 RANDOM VARIABLES

    std::shared_ptr<RandomVariableNode> state(
        make_shared<RandomVariableNode>(state_metadata));
    state->add_cpd_template(state_prior_cpd);
    state->add_cpd_template(state_transition_cpd);

    std::shared_ptr<RandomVariableNode> light(
        make_shared<RandomVariableNode>(light_metadata));
    light->add_cpd_template(light_emission_cpd);

    std::shared_ptr<RandomVariableNode> room(
        make_shared<RandomVariableNode>(room_metadata));
    room->add_cpd_template(room_emission_cpd);

    std::shared_ptr<RandomVariableNode> tg(
        make_shared<RandomVariableNode>(tg_metadata));
    tg->add_cpd_template(tg_emission_cpd);

    std::shared_ptr<RandomVariableNode> ty(
        make_shared<RandomVariableNode>(ty_metadata));
    ty->add_cpd_template(ty_emission_cpd);

    // 3 CONNECTIONS
    for (int i = 0; i < 7; i++) {
        state_metadata->add_parent_link(theta_s_metadatas[i], true);
        light_metadata->add_parent_link(pi_lt_metadatas[i], false);
    }
    state_metadata->add_parent_link(state_metadata, true);
    light_metadata->add_parent_link(state_metadata, false);
    room_metadata->add_parent_link(state_metadata, false);
    tg_metadata->add_parent_link(state_metadata, false);
    ty_metadata->add_parent_link(state_metadata, false);

    // 4 DBN
    DynamicBayesNet model;

    // 4.1 ADD PARAMETER NODES
    for (int i = 0; i < 7; i++) {
        model.add_node_template(*theta_s_nodes[i]);
        model.add_node_template(*pi_lt_nodes[i]);
    }

    // 4.2 ADD VARIABLE NODES
    model.add_node_template(*state);
    model.add_node_template(*light);
    model.add_node_template(*room);
    model.add_node_template(*tg);
    model.add_node_template(*ty);

    return model;
}

void test_training_runtime() {
    shared_ptr<DynamicBayesNet> model =
        make_shared<DynamicBayesNet>(create_tomcat_model());

    Tensor3 light_data =
        read_tensor_from_file("../../data/samples/asist/sparky/lights.csv");
    Tensor3 room_data =
        read_tensor_from_file("../../data/samples/asist/sparky/rooms.csv");
    Tensor3 tg_data = read_tensor_from_file(
        "../../data/samples/asist/sparky/triaging_green.csv");
    Tensor3 ty_data = read_tensor_from_file(
        "../../data/samples/asist/sparky/triaging_yellow.csv");
    EvidenceSet data;
    data.add_data("Light", light_data);
    data.add_data("Room", room_data);
    data.add_data("TG", tg_data);
    data.add_data("TY", ty_data);

    model->unroll(data.get_time_steps(), true);
    shared_ptr<GibbsSampler> gibbs = make_shared<GibbsSampler>(model, 500);
    gibbs->set_num_in_plate_samples(data.get_num_data_points());

    Timer timer;
    shared_ptr<gsl_rng> gen(gsl_rng_alloc(gsl_rng_mt19937));
    DBNSamplingTrainer trainer(gen, gibbs, 5000);
    trainer.fit(data);

    shared_ptr<DBNSaver> saver =
        make_shared<DBNSaver>(model, "../../data/model/asist/sparky");
    saver->save();
}

int main() {
    //generate_student_data();
    //test_student_training_runtime();
    //test_training_runtime();

    //test_message_passing();
    test_pipeline();

    //    test_cpp_capabilities();
    // test_random_number_generation();
    //    test_dbn_entities();

//        shared_ptr<gsl_rng> gen(gsl_rng_alloc(gsl_rng_mt19937));
//    //
//        shared_ptr<DynamicBayesNet> dbn_ptr =
//            make_shared<DynamicBayesNet>(create_dbn(true));
//        dbn_ptr->unroll(20, true);
//        generate_samples_to_test(dbn_ptr, 10, gen);

    //        shared_ptr<DynamicBayesNet> dbn_ptr =
    //            make_shared<DynamicBayesNet>(create_dbn(false));
    //    dbn_ptr->unroll(100, true);
    //
    //    train_dbn(dbn_ptr, 50, gen);

    //    EvidenceSet data("../../data/samples/toy");
    //    cout << data.get_num_data_points() << endl;
    //    cout << data.get_time_steps() << endl;
    //    //cout << data["TG"];
    //
    //    KFold k_fold(5);
    //    int fold = 1;
    //    for(auto&[training, test] : k_fold.split(gen, data)) {
    //        cout << "Fold " << fold++ << endl;
    //        cout << training["TG"] << endl;
    //        cout << test["TG"] << endl;
    //    }

    //    test_baseline_estimator(dbn_ptr);
    //    test_sum_product_estimator(dbn_ptr);

    //    double* buffer = new double[12];
    //
    //    buffer[0] = 1;
    //    buffer[1] = 2;
    //    buffer[2] = 3;
    //    buffer[3] = 4;
    //    buffer[4] = 5;
    //    buffer[5] = 6;
    //    buffer[6] = 7;
    //    buffer[7] = 8;
    //    buffer[8] = 9;
    //    buffer[9] = 10;
    //    buffer[10] = 11;
    //    buffer[11] = 12;
    //    Tensor3 tensor(buffer, 3, 2, 2);
    //    LOG("Original");
    //    LOG(tensor);
    //    LOG("Repeated axis = 0");
    //    LOG(tensor.repeat(1, 0));
    //    LOG("Repeated axis = 1");
    //    LOG(tensor.repeat(1, 1));
    //    LOG("Repeated axis = 2");
    //    LOG(tensor.repeat(1, 2));

    // test_pipeline();
    // test_mosquitto();
    // test_message_passing();

    //    Eigen::MatrixXd m(2,3);
    //    m << 1, 0, 1,
    //         0, 0, 1;
    //
    //    Eigen::MatrixXd b = (m.array() ==
    //    1).select(MatrixXd::Constant(m.rows(), m.cols(), 2),
    //    MatrixXd::Zero(m.rows(), m.cols()));
    //
    //    cout << b;

    // gsl_rng_set(gen.get(), time(0));
    // dbn.unroll(4, true);

    // dbn.unroll(4, true);
    // AncestralSampler sampler(dbn, gen);
    // Tensor3 state_data =
    // read_tensor_from_file("../../data/samples/State.txt");
    // sampler.set_num_in_plate_samples(state_data.get_shape()[1]);
    // sampler.add_data("State", state_data);
    // sampler.sample(5);
    // sampler.save_samples_to_folder("../../data/samples");

    //
    //    cout << "States" << endl;
    //    cout << sampler.get_samples("State") << endl;
    //    cout << "TGs" << endl;
    //    cout << sampler.get_samples("TG") << endl;
    //    cout << "TYs" << endl;
    //    cout << sampler.get_samples("TY") << endl;

    //    cout << "PriorS" << endl;
    //    cout << sampler.get_samples("PriorS") << endl;

    //    sampler.get_dbn().save_to_folder("../../data/model");
    //
    //    char buff[FILENAME_MAX]; // create string buffer to hold path
    //    getcwd(buff, FILENAME_MAX);
    //    fs::path current_dir = buff;
    //
    //    fs::path folder = current_dir.parent_path().parent_path() /
    //                                   fs::path("data/samples");
    //
    //    fs::path filepath = folder / fs::path("test.txt");
    //
    //    cout << filepath;
    //    ofstream file("../../data/samples/test2.txt");
    //    cout << "Writing into a file";
    //    file << "Teste Maior";
    //    file.close();

    //    for(int i = 0; i < 6000000; i++){
    //        cout << i << '\n';
    //    }
}
