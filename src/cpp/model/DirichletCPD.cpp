#include "DirichletCPD.h"
#include "ConstantVectorNode.h"

namespace tomcat {
    namespace model {

        DirichletCPD::DirichletCPD(
            std::vector<std::string> parent_node_label_order,
            Eigen::MatrixXd& parameter_table)
            : CPD<Eigen::MatrixXd>(std::move(parent_node_label_order)) {

            for (int row = 0; row < parameter_table.rows(); row++) {
                std::vector<double> alpha;
                alpha.reserve(parameter_table.cols());

                for (int col = 0; col < parameter_table.cols(); col++) {
                    double coefficient = parameter_table(row, col);
                    alpha.push_back(coefficient);
                }

                // Create a vector containing the alpha coefficients of the
                // current row and use it to create a constant vector node
                ConstantVectorNode alpha_node(std::move(alpha));
                std::unique_ptr<Node<std::vector<double>>> alpha_ptr =
                    std::make_unique<ConstantVectorNode>(std::move(alpha_node));
                this->parameter_table.push_back(std::move(alpha_ptr));
            }
        }

        // todo - implement sampling from a dirichlet distribution
        Eigen::MatrixXd DirichletCPD::sample() const {
            //samples.resize(this->parameter_table.size(), this->parameter_table[0].);
            return Eigen::MatrixXd::Random(2,2);
        }

        void DirichletCPD::print(std::ostream& os) const {
            os << "Dirichlet CPD: {\n";
            for (auto& alpha : this->parameter_table) {
                os << " " << *alpha << "\n";
            }
            os << "}";
        }

    } // namespace model
} // namespace tomcat