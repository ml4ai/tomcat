#include "CategoricalCPD.h"
#include "DirichletCPD.h"
#include "DynamicBayesNet.h"
#include "GaussianCPD.h"
#include "Node.h"
#include "NodeMetadata.h"
#include "RandomVariableNode.h"
#include <eigen3/Eigen/Dense>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_rng.h>
#include <iostream>
#include <memory>
#include <variant>

using namespace Eigen;
using namespace tomcat::model;

class A {
  public:
    A() { std::cout << "New A" << std::endl; }
    A(A& a) { std::cout << "Copying A" << std::endl; }
    A(A&& a) { std::cout << "Moving A" << std::endl; }
};

class B {
  private:
    A a;

  public:
    B() { std::cout << "New B without A" << std::endl; }
    // This yields two moves
    // This is ambiguous with the two below
    B(A a) : a(std::move(a)) { std::cout << "New B with A" << std::endl; }
    // This yields one copy
    // B(A& a) : a(a) { std::cout << "New B with A&" << std::endl; }
    // This yields one move
    // B(A&& a) : a(std::move(a)) { std::cout << "New B with A&&" << std::endl;
    // }
    void print() const { std::cout << "I am B" << std::endl; }
};

int main() {
    // gsl_rng* gen = gsl_rng_alloc(gsl_rng_mt19937);
    std::shared_ptr<gsl_rng> gen(gsl_rng_alloc(gsl_rng_mt19937));
    gsl_rng_set(gen.get(), time(0));
    unsigned int k = gsl_ran_poisson(gen.get(), 3);
    std::cout << k << std::endl;

    VectorXd v(2);
    v << 0.54, 0.46;
    std::cout << v << std::endl;

    double* theta = new double[2];
    double* alpha = v.data();
    gsl_ran_dirichlet(gen.get(), 2, alpha, theta);
    for (int i = 0; i < 2; i++) {
        std::cout << theta[i] << " ";
    }
    std::cout << std::endl;

    VectorXd v2 = Map<VectorXd>(theta, 2);
    std::cout << v2 << std::endl;

    MatrixXd m(2, 2);
    m(0, 0) = 0.3;
    m(1, 0) = 0.8;
    m(0, 1) = 1 - m(0, 0);
    m(1, 1) = 1 - m(1, 0);
    std::cout << m << std::endl;

    // Testing node metadata
    NodeMetadata metadata1;
    metadata1.label = "StateA";
    metadata1.initial_time_step = 0;
    metadata1.repeatable = true;
    metadata1.cardinality = 2;

    NodeMetadata metadata2;
    metadata2.label = "StateB";
    metadata2.initial_time_step = 0;
    metadata2.repeatable = true;
    metadata2.cardinality = 2;

    std::cout << metadata2 << std::endl;

    // Testing constant nodes
    Node node1(2);
    Node node2(v);

    std::cout << node1 << std::endl;
    std::cout << node2 << std::endl;

    // Testing CPDs
    std::vector<std::string> order{"A", "B", "C"};
    CategoricalCPD categorical_cpd(order, m);
    std::cout << categorical_cpd << std::endl;
    Eigen::VectorXd sample = categorical_cpd.sample(gen);
    std::cout << sample << std::endl;

    GaussianCPD gaussian_cpd(order, m);
    std::cout << gaussian_cpd << std::endl;
    sample = gaussian_cpd.sample(gen);
    std::cout << sample << std::endl;

    DirichletCPD dirichlet_cpd(order, m);
    std::cout << dirichlet_cpd << std::endl;
    Eigen::MatrixXd sample2 = dirichlet_cpd.sample(gen);
    std::cout << sample2 << std::endl;

    // Testing RV nodes
    RandomVariableNode param_node1(
        std::make_shared<NodeMetadata>(metadata1),
        std::make_unique<CategoricalCPD>(categorical_cpd));
    std::cout << param_node1 << std::endl;

    RandomVariableNode param_node2(
        std::make_shared<NodeMetadata>(metadata2),
        std::make_unique<CategoricalCPD>(categorical_cpd));
    std::cout << param_node2 << std::endl;

    NodeMetadata metadata3 = metadata2;
    metadata3.label = "StateC";
    metadata3.add_parent_link(std::make_shared<RandomVariableNode>(param_node1), true);
    metadata3.add_parent_link(std::make_shared<RandomVariableNode>(param_node2), false);

    RandomVariableNode data_node1(
        std::make_shared<NodeMetadata>(metadata3),
        std::make_unique<CategoricalCPD>(categorical_cpd));
    std::cout << data_node1 << std::endl;

    DynamicBayesNet dbn;
    dbn.add_node(std::make_unique<RandomVariableNode>(param_node1));
    dbn.add_node(std::make_unique<RandomVariableNode>(param_node2));
    dbn.add_node(std::make_unique<RandomVariableNode>(data_node1));
    dbn.unroll(3);

    //    std::vector<std::variant<A*, B*>> multi_vec;
    //    std::variant<A*, B*> var1 (new A());
    //    multi_vec.push_back(var1);

    // multi_vec.push_back(B());
}
