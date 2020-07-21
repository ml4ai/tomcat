#include "DirichletCPD.h"

namespace tomcat {
    namespace model {
        DirichletCPD::DirichletCPD(
            std::vector<std::string> parent_node_label_order,
            std::vector<Node> parameter_table)
            : ContinuousCPD(std::move(parent_node_label_order)) {

            // Each node contains the parameter vector alpha.
            for (int i = 0; i < parameter_table.size(); i++) {
                std::vector<Node> parameters = {parameter_table[i]};
                this->parameter_table.push_back(std::move(parameters));
            }
        }

        DirichletCPD::DirichletCPD(
            std::vector<std::string> parent_node_label_order,
            Eigen::MatrixXd& parameter_values)
            : ContinuousCPD(std::move(parent_node_label_order)) {

            this->init_from_matrix(parameter_values);
        }

        void DirichletCPD::init_from_matrix(Eigen::MatrixXd& parameter_values) {
            this->alpha_size = parameter_values.cols();

            for (int row = 0; row < parameter_values.rows(); row++) {
                Eigen::VectorXd alpha = parameter_values.row(row);
                std::vector<Node> alpha_vector = {Node(alpha)};
                this->parameter_table.push_back(std::move(alpha_vector));
            }
        }

        Eigen::MatrixXd
        DirichletCPD::sample(std::shared_ptr<gsl_rng> generator) const {
            int rows = this->parameter_table.size();
            double* sample_ptr = new double[rows * this->alpha_size];

            for (int i = 0; i < rows; i++) {
                const double* alpha =
                    this->parameter_table[i][0].get_assignment().data();

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
                os << " " << alpha[0] << "\n";
            }
            os << "}";
        }

    } // namespace model
} // namespace tomcat