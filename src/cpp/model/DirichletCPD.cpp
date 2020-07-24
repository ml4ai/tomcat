#include "DirichletCPD.h"
#include "ConstantNode.h"

namespace tomcat {
    namespace model {
        void DirichletCPD::init_from_table(
            std::vector<std::shared_ptr<Node>>& parameter_table) {
            // Each node contains the parameter vector alpha.
            for (int i = 0; i < parameter_table.size(); i++) {
                std::vector<std::shared_ptr<Node>> parameters;
                parameters.push_back(parameter_table[i]);
                this->parameter_table.push_back(std::move(parameters));
            }
        }

        void DirichletCPD::init_from_matrix(
            const Eigen::MatrixXd& matrix) {
            this->alpha_size = matrix.cols();

            for (int row = 0; row < matrix.rows(); row++) {
                Eigen::VectorXd alpha = matrix.row(row);
                std::vector<std::shared_ptr<Node>> alpha_vector;
                alpha_vector.push_back(
                    std::make_shared<ConstantNode>(ConstantNode(alpha)));
                this->parameter_table.push_back(std::move(alpha_vector));
            }
        }

        Eigen::MatrixXd
        DirichletCPD::sample(std::shared_ptr<gsl_rng> generator) const {
            int rows = this->parameter_table.size();
            double* sample_ptr = new double[rows * this->alpha_size];

            for (int i = 0; i < rows; i++) {
                const double* alpha =
                    this->parameter_table[i][0]->get_assignment().data();

                gsl_ran_dirichlet(generator.get(),
                                  this->parameter_table.size(),
                                  alpha,
                                  sample_ptr);
                sample_ptr += this->alpha_size;
            }

            sample_ptr -= rows * this->alpha_size;

            Eigen::Map<Eigen::Matrix<double,
                                     Eigen::Dynamic,
                                     Eigen::Dynamic,
                                     Eigen::RowMajor>>
                samples(sample_ptr, rows, this->alpha_size);
            return samples;
        }

        void DirichletCPD::print(std::ostream& os) const {
            os << "Dirichlet CPD: {\n";
            for (auto& alpha : this->parameter_table) {
                os << " " << *alpha[0] << "\n";
            }
            os << "}";
        }

        std::unique_ptr<CPD> DirichletCPD::clone() const {
            return std::make_unique<DirichletCPD>(*this);
        }

    } // namespace model
} // namespace tomcat