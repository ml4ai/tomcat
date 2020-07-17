#include "DiscreteCPD.h"
#include "ConstantVectorNode.h"

namespace tomcat {
    namespace model {

        DiscreteCPD::DiscreteCPD(
            std::vector<std::string> parent_node_label_order,
            Eigen::MatrixXd& cpd_table)
            : CPD<double>(std::move(parent_node_label_order)) {

            for (int row = 0; row < cpd_table.rows(); row++) {
                std::vector<double> probabilities;
                probabilities.reserve(cpd_table.cols());
                for (int col = 0; col < cpd_table.cols(); col++) {
                    double probability = cpd_table(row, col);
                    probabilities.push_back(probability);
                }

                // Create a vector containing the probabilities of the current
                // row and use it to create a constant vector node
                std::unique_ptr<Node<std::vector<double>>> node =
                    std::make_unique<ConstantVectorNode>(
                        ConstantVectorNode(std::move(probabilities)));
                this->cpd_table.push_back(std::move(node));
            }
        }

        double DiscreteCPD::sample() const { return 2.5; }

        void DiscreteCPD::print(std::ostream& os) const {
            os << "CPD: {\n";
            for (auto& row : this->cpd_table) {
                os << *row << "\n";
            }
            os << "}";
        }

    } // namespace model
} // namespace tomcat