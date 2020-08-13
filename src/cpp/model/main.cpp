#include "AncestralSampler.h"
#include "CategoricalCPD.h"
#include "ConstantNode.h"
#include "DirichletCPD.h"
#include "DynamicBayesNet.h"
#include "FileHandler.h"
#include "GaussianCPD.h"
#include "Node.h"
#include "NodeMetadata.h"
#include "RandomVariableNode.h"
#include "distribution/Categorical.h"
#include "distribution/Dirichlet.h"
#include "distribution/Gaussian.h"
#include <eigen3/Eigen/Dense>
#include <fstream>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_rng.h>
#include <iostream>
#include <memory>
#include <unistd.h>
#include <variant>

#include <boost/filesystem.hpp>
namespace fs = boost::filesystem;

using namespace Eigen;
using namespace tomcat::model;

class A {
  public:
    A() { std::cout << "New A" << std::endl; }
    A(A& a) { std::cout << "Copying A" << std::endl; }
    A(A&& a) { std::cout << "Moving A" << std::endl; }

    void print() { std::cout << "I am A" << std::endl; }
};

class B {
  private:
    A a;

  public:
    B() { std::cout << "New B without A" << std::endl; }
    B(A& a) : a(a) {
        std::cout << "New B with A&" << std::endl;
        print(a);
    }
    B(A&& a) : a(std::move(a)) {
        std::cout << "New B with A&&" << std::endl;
        print(a);
    }
    void print(A& a) const { a.print(); }
};

int main() {
    //          A a;
    //          B b(std::move(a));
    //    //
    //          std::shared_ptr<A> a_ptr = std::make_shared<A>();
    //    //      A a_ref = *a_ptr;
    //          b.print(*a_ptr);

    gsl_rng* gen_ptr = gsl_rng_alloc(gsl_rng_mt19937);
    gsl_rng_set(gen_ptr, time(0));
    double* probs = new double[3]{0.3, 0.4, 0.3};
    unsigned int* sample = new unsigned int[3];
    gsl_ran_multinomial(gen_ptr, 3, 1, probs, sample);

    std::cout << sample[0] << "," << sample[1] << "," << sample[2] << std::endl;

    unsigned int x = std::distance(sample, std::find(sample, sample + 3, 1));
    std::cout << x << std::endl;

    delete[] probs;
    delete[] sample;
    delete gen_ptr;

    //    gsl_rng_set(gen.get(), time(0));
    //    unsigned int k = gsl_ran_poisson(gen.get(), 3);
    //    std::cout << k << std::endl;
    //
    //    VectorXd v(2);
    //    v << 0.54, 0.46;
    //    std::cout << v << std::endl;
    //
    //    double* theta = new double[2];
    //    double* alpha = v.data();
    //    gsl_ran_dirichlet(gen.get(), 2, alpha, theta);
    //    for (int i = 0; i < 2; i++) {
    //        std::cout << theta[i] << " ";
    //    }
    //    std::cout << std::endl;
    //
    //    VectorXd v2 = Map<VectorXd>(theta, 2);
    //    std::cout << v2 << std::endl;
    //
    //    MatrixXd m(2, 2);
    //    m(0, 0) = 0.3;
    //    m(1, 0) = 0.8;
    //    m(0, 1) = 1 - m(0, 0);
    //    m(1, 1) = 1 - m(1, 0);
    //    std::cout << m << std::endl;
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
    //    std::cout << metadata2 << std::endl;
    //
    //    // Testing constant nodes
    //    ConstantNode node1(2);
    //    ConstantNode node2(v, "v");
    //
    //    std::cout << node1 << std::endl;
    //    std::cout << node2 << std::endl;
    //
    //    // Testing CPDs
    //    std::vector<std::string> order{"A", "B", "C"};
    //    CategoricalCPD categorical_cpd(order, m);
    //    std::cout << categorical_cpd << std::endl;
    //    Eigen::VectorXd sample = categorical_cpd.sample(gen);
    //    std::cout << sample << std::endl;
    //
    //    GaussianCPD gaussian_cpd(order, m);
    //    std::cout << gaussian_cpd << std::endl;
    //    sample = gaussian_cpd.sample(gen);
    //    std::cout << sample << std::endl;
    //
    //    DirichletCPD dirichlet_cpd(order, m);
    //    std::cout << dirichlet_cpd << std::endl;
    //    Eigen::MatrixXd sample2 = dirichlet_cpd.sample(gen);
    //    std::cout << sample2 << std::endl;
    //
    //    // Testing RV nodes
    //    RandomVariableNode param_node1(
    //        std::make_shared<NodeMetadata>(metadata1),
    //        std::make_unique<CategoricalCPD>(categorical_cpd));
    //    std::cout << param_node1 << std::endl;
    //
    //    RandomVariableNode param_node2(
    //        std::make_shared<NodeMetadata>(metadata2),
    //        std::make_unique<CategoricalCPD>(categorical_cpd));
    //    std::cout << param_node2 << std::endl;
    //
    //    NodeMetadata metadata3 = metadata2;
    //    metadata3.label = "StateC";
    //    metadata3.add_parent_link(std::make_shared<RandomVariableNode>(param_node1),
    //                              true);
    //    metadata3.add_parent_link(std::make_shared<RandomVariableNode>(param_node2),
    //                              false);
    //
    //    RandomVariableNode data_node1(
    //        std::make_shared<NodeMetadata>(metadata3),
    //        std::make_unique<CategoricalCPD>(categorical_cpd));
    //    std::cout << data_node1 << std::endl;
    //
    //    DynamicBayesNet dbn;
    //    dbn.add_node(param_node1);
    //    dbn.add_node(param_node2);
    //    dbn.add_node(data_node1);
    // dbn.unroll(3);

    //    std::vector<std::variant<A*, B*>> multi_vec;
    //    std::variant<A*, B*> var1 (new A());
    //    multi_vec.push_back(var1);

    // multi_vec.push_back(B());

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

    std::shared_ptr<NodeMetadata> state_prior_metadata_ptr =
        std::make_shared<NodeMetadata>(std::move(state_prior_metadata));

    std::shared_ptr<NodeMetadata> theta_s0_metadata_ptr =
        std::make_shared<NodeMetadata>(std::move(theta_s0_metadata));

    std::shared_ptr<NodeMetadata> theta_s1_metadata_ptr =
        std::make_shared<NodeMetadata>(std::move(theta_s1_metadata));

    std::shared_ptr<NodeMetadata> theta_s2_metadata_ptr =
        std::make_shared<NodeMetadata>(std::move(theta_s2_metadata));

    Eigen::MatrixXd prior_state_prior(1, 3);
    prior_state_prior << 1, 0.000001, 0.000001;
    DirichletCPD prior_state_prior_cpd({}, std::move(prior_state_prior));

    Eigen::MatrixXd prior_theta_s0(1, 3);
    prior_theta_s0 << 0.2, 0.3, 0.5;
    DirichletCPD prior_theta_s0_cpd({}, std::move(prior_theta_s0));

    Eigen::MatrixXd prior_theta_s1(1, 3);
    prior_theta_s1 << 0.4, 0.1, 0.5;
    DirichletCPD prior_theta_s1_cpd({}, std::move(prior_theta_s1));

    Eigen::MatrixXd prior_theta_s2(1, 3);
    prior_theta_s2 << 0.7, 0.1, 0.2;
    DirichletCPD prior_theta_s2_cpd({}, std::move(prior_theta_s2));

    std::shared_ptr<CPD> prior_state_prior_cpd_ptr =
        std::make_shared<DirichletCPD>(prior_state_prior_cpd);
    std::shared_ptr<CPD> prior_theta_s0_cpd_ptr =
        std::make_shared<DirichletCPD>(prior_theta_s0_cpd);
    std::shared_ptr<CPD> prior_theta_s1_cpd_ptr =
        std::make_shared<DirichletCPD>(prior_theta_s1_cpd);
    std::shared_ptr<CPD> prior_theta_s2_cpd_ptr =
        std::make_shared<DirichletCPD>(prior_theta_s2_cpd);

    RandomVariableNode prior_state_node(state_prior_metadata_ptr);
    prior_state_node.add_cpd(prior_state_prior_cpd_ptr);

    RandomVariableNode theta_s0_node(theta_s0_metadata_ptr);
    theta_s0_node.add_cpd(prior_theta_s0_cpd_ptr);

    RandomVariableNode theta_s1_node(theta_s1_metadata_ptr);
    theta_s1_node.add_cpd(prior_theta_s1_cpd_ptr);

    RandomVariableNode theta_s2_node(theta_s2_metadata_ptr);
    theta_s2_node.add_cpd(prior_theta_s2_cpd_ptr);

    std::shared_ptr<RandomVariableNode> prior_state_node_ptr =
        std::make_shared<RandomVariableNode>(prior_state_node);
    std::shared_ptr<RandomVariableNode> theta_s0_node_ptr =
        std::make_shared<RandomVariableNode>(theta_s0_node);
    std::shared_ptr<RandomVariableNode> theta_s1_node_ptr =
        std::make_shared<RandomVariableNode>(theta_s1_node);
    std::shared_ptr<RandomVariableNode> theta_s2_node_ptr =
        std::make_shared<RandomVariableNode>(theta_s2_node);

    NodeMetadata state_metadata =
        NodeMetadata::create_multiple_time_link_metadata(
            "State", true, false, true, 0, 1, 3);

    std::shared_ptr<NodeMetadata> state_metadata_ptr =
        std::make_shared<NodeMetadata>(state_metadata);

    CategoricalCPD prior_state_cpd(
        {}, {std::make_shared<Categorical>(Categorical(prior_state_node_ptr))});

    std::shared_ptr<CPD> prior_state_cpd_ptr =
        std::make_shared<CategoricalCPD>(prior_state_cpd);

    CategoricalCPD state_cpd(
        {state_metadata_ptr},
        {std::make_shared<Categorical>(theta_s0_node_ptr),
         std::make_shared<Categorical>(theta_s1_node_ptr),
         std::make_shared<Categorical>(theta_s2_node_ptr)});
    //    Eigen::MatrixXd state_transition_matrix(3, 3);
    //    state_transition_matrix << 0, 0, 1, 1, 0, 0, 0, 1, 0;
    //    CategoricalCPD state_cpd({"State"},
    //    std::move(state_transition_matrix));

    std::shared_ptr<CPD> state_cpd_ptr =
        std::make_shared<CategoricalCPD>(state_cpd);

    RandomVariableNode state_node(state_metadata_ptr);
    state_node.add_cpd(prior_state_cpd_ptr);
    state_node.add_cpd(state_cpd_ptr);

    std::shared_ptr<RandomVariableNode> state_node_ptr =
        std::make_shared<RandomVariableNode>(state_node);
    state_metadata_ptr->add_parent_link(state_metadata_ptr, true);
    state_metadata_ptr->add_parent_link(state_prior_metadata_ptr, false);
    state_metadata_ptr->add_parent_link(theta_s0_metadata_ptr, true);
    state_metadata_ptr->add_parent_link(theta_s1_metadata_ptr, true);
    state_metadata_ptr->add_parent_link(theta_s2_metadata_ptr, true);

    NodeMetadata tg_metadata = NodeMetadata::create_multiple_time_link_metadata(
        "TG", true, false, true, 1, 1, 2);
    tg_metadata.add_parent_link(state_metadata_ptr, false);

    Eigen::MatrixXd tg_emission_matrix(3, 2);
    tg_emission_matrix << 1, 0, 0, 1, 0.5, 0.5;
    CategoricalCPD tg_cpd({state_metadata_ptr}, std::move(tg_emission_matrix));

    RandomVariableNode tg_node(std::make_shared<NodeMetadata>(tg_metadata));
    tg_node.add_cpd(std::make_shared<CategoricalCPD>(tg_cpd));

    NodeMetadata ty_metadata = NodeMetadata::create_multiple_time_link_metadata(
        "TY", true, false, true, 1, 1, 2);
    ty_metadata.add_parent_link(state_metadata_ptr, false);

    Eigen::MatrixXd ty_emission_matrix(3, 2);
    ty_emission_matrix << 0, 1, 1, 0, 0.5, 0.5;
    CategoricalCPD ty_cpd({state_metadata_ptr}, std::move(ty_emission_matrix));

    RandomVariableNode ty_node(std::make_shared<NodeMetadata>(ty_metadata));
    ty_node.add_cpd(std::make_shared<CategoricalCPD>(ty_cpd));

    DynamicBayesNet dbn(3);
    dbn.add_node_template(state_node);
    dbn.add_node_template(tg_node);
    dbn.add_node_template(ty_node);
    dbn.add_node_template(prior_state_node);
    dbn.add_node_template(std::move(theta_s0_node));
    dbn.add_node_template(std::move(theta_s1_node));
    dbn.add_node_template(std::move(theta_s2_node));

    dbn.unroll(3, false);

    Node::NodeMap param_map;
    Eigen::VectorXd temp(3);
    temp << 0.000001, 0.000001, 1;
    param_map["(PriorS,0)"] =
        std::make_shared<ConstantNode>(ConstantNode(temp));
    prior_state_cpd_ptr->update_dependencies(param_map, 0);

    //    std::vector<std::shared_ptr<RandomVariableNode>> nodes =
    //        dbn.get_nodes_topological_order();
    //
    //    for (const auto& node : nodes) {
    //        std::cout << *node << std::endl;
    //    }

    std::shared_ptr<gsl_rng> gen(gsl_rng_alloc(gsl_rng_mt19937));
    dbn.unroll(10, true);
    // dbn.load_from_folder("../../data/model");
    AncestralSampler sampler(dbn, gen);
    Tensor3 state_data = read_tensor_from_file("../../data/samples/State.txt");
    sampler.add_data("State", state_data);
    sampler.sample(5);
    //    sampler.sample(5, 10);

    std::cout << "States" << std::endl;
    std::cout << sampler.get_samples("State") << std::endl;
    std::cout << "TGs" << std::endl;
    std::cout << sampler.get_samples("TG") << std::endl;
    std::cout << "TYs" << std::endl;
    std::cout << sampler.get_samples("TY") << std::endl;
    //    std::cout << "PriorS" << std::endl;
    //    std::cout << sampler.get_samples("PriorS") << std::endl;

    //    sampler.save_samples_to_folder("../../data/samples");
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
    //    std::cout << filepath;
    //    std::ofstream file("../../data/samples/test2.txt");
    //    std::cout << "Writing into a file";
    //    file << "Teste Maior";
    //    file.close();
}
