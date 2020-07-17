#include "ConstantNumericNode.h"
#include "ConstantVectorNode.h"
#include "DiscreteCPD.h"
#include "NodeMetadata.h"
#include "RandomVariableNumericNode.h"
#include <eigen3/Eigen/Dense>
#include <iostream>

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
    //B(A& a) : a(a) { std::cout << "New B with A&" << std::endl; }
    // This yields one move
    //B(A&& a) : a(std::move(a)) { std::cout << "New B with A&&" << std::endl; }
    void print() const { std::cout << "I am B" << std::endl; }
};

int main() {
        MatrixXd m(2, 2);
    m(0, 0) = 3;
    m(1, 0) = 2.5;
    m(0, 1) = -1;
    m(1, 1) = m(1, 0) + m(0, 1);
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

    NodeMetadata metadata3 = metadata2;
    metadata3.label = "StateC";
    metadata3.add_parent_link(metadata1, true);
    metadata3.add_parent_link(metadata2, false);

    std::cout << metadata2 << std::endl;
    std::cout << metadata3 << std::endl;

    // Testing constant nodes
    ConstantNumericNode node1(2);
    ConstantVectorNode node2({0.2, 0.3, 0.5});

    std::cout << node1 << std::endl;
    std::cout << node2 << std::endl;

    // Testing CPDs
    std::vector<std::string> order{"A", "B", "C"};
    DiscreteCPD constant_cpd(std::move(order), m);
    std::cout << constant_cpd << std::endl;

    // Testing RV nodes
    std::shared_ptr<NodeMetadata> metadata = std::make_shared<NodeMetadata>(metadata3);
    std::unique_ptr<DiscreteCPD> cpd = std::make_unique<DiscreteCPD>(std::move(constant_cpd));

    RandomVariableNumericNode rv_node1(
        std::move(metadata),
        std::move(cpd));
    std::cout << rv_node1 << std::endl;
    std::cout << *rv_node1.get_metadata() << std::endl;
    std::cout << *rv_node1.get_cpd() << std::endl;
}
