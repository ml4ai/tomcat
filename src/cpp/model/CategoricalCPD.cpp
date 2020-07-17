#include "CategoricalCPD.h"
#include "ConstantVectorNode.h"

namespace tomcat {
    namespace model {

        CategoricalCPD::CategoricalCPD(
            std::vector<std::string> parent_node_label_order,
            Eigen::MatrixXd& cpd_table)
            : CPD<Eigen::MatrixXd>(std::move(parent_node_label_order)) {

            for (int row = 0; row < cpd_table.rows(); row++) {
                std::vector<double> probabilities;
                probabilities.reserve(cpd_table.cols());

                for (int col = 0; col < cpd_table.cols(); col++) {
                    double probability = cpd_table(row, col);
                    probabilities.push_back(probability);
                }

                // Create a vector containing the probabilities of the current
                // row and use it to create a constant vector node
                ConstantVectorNode probabilities_node(std::move(probabilities));
                std::unique_ptr<Node<std::vector<double>>> probabilities_ptr =
                    std::make_unique<ConstantVectorNode>(
                        std::move(probabilities_node));
                this->cpd_table.push_back(std::move(probabilities_ptr));
            }
        }

        // todo - sample from a categorical distribution
        Eigen::MatrixXd CategoricalCPD::sample() const {
            Eigen::VectorXd samples(this->cpd_table.size());
            return samples;
        }

        void CategoricalCPD::print(std::ostream& os) const {
            os << "Categorical CPD: {\n";
            for (auto& probabilities : this->cpd_table) {
                os << " " << *probabilities << "\n";
            }
            os << "}";
        }

    } // namespace model
} // namespace tomcat