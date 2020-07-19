#include "DirichletCPD.h"

namespace tomcat {
    namespace model {

        DirichletCPD::DirichletCPD(
            std::vector<std::string> parent_node_label_order,
            Eigen::MatrixXd& parameter_table)
            : CPD(std::move(parent_node_label_order)) {

            typedef Node<Eigen::VectorXd> VectorNode;

            for (int row = 0; row < parameter_table.rows(); row++) {
                Eigen::VectorXd alpha(parameter_table.cols());

                for (int col = 0; col < parameter_table.cols(); col++) {
                    alpha(col) = parameter_table(row, col);
                }

                VectorNode alpha_node(std::move(alpha));
                std::unique_ptr<VectorNode> alpha_ptr =
                    std::make_unique<VectorNode>(std::move(alpha_node));
                this->parameter_table.push_back(std::move(alpha_ptr));
            }
        }

        Eigen::MatrixXd
        DirichletCPD::sample(std::shared_ptr<gsl_rng> generator) const {
            int rows = static_cast<int>(this->parameter_table.size());
            int cols = static_cast<int>(
                this->parameter_table[0]->get_assignment().size());

            double* sample_ptr = new double[rows * cols];
            for (int i = 0; i < this->parameter_table.size(); i++) {
                double* alpha = static_cast<Eigen::VectorXd>(
                                    this->parameter_table[i]->get_assignment())
                                    .data();

                gsl_ran_dirichlet(generator.get(),
                                  this->parameter_table.size(),
                                  alpha,
                                  sample_ptr);
                sample_ptr += cols;
            }

            sample_ptr -= rows * cols;

            Eigen::Map<Eigen::Matrix<double,
                                     Eigen::Dynamic,
                                     Eigen::Dynamic,
                                     Eigen::RowMajor>>
                samples(sample_ptr, rows, cols);
            return samples;
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