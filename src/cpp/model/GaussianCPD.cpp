#include "GaussianCPD.h"

namespace tomcat {
    namespace model {

        GaussianCPD::GaussianCPD(
            std::vector<std::string> parent_node_label_order,
            Eigen::MatrixXd& parameter_table)
            : CPD(std::move(parent_node_label_order)) {

            typedef Node<double> NumericNode;

            for (int row = 0; row < parameter_table.rows(); row++) {
                double mean = parameter_table(row, 0);

                NumericNode mean_node(mean);
                std::unique_ptr<NumericNode> mean_ptr =
                    std::make_unique<NumericNode>(std::move(mean_node));

                double variance = parameter_table(row, 1);
                NumericNode variance_node(variance);
                std::unique_ptr<NumericNode> variance_ptr =
                    std::make_unique<NumericNode>(
                        std::move(variance_node));

                GaussianParameters parameters(std::move(mean_ptr),
                                              std::move(variance_ptr));
                this->parameter_table.push_back(std::move(parameters));
            }
        }

        Eigen::MatrixXd
        GaussianCPD::sample(std::shared_ptr<gsl_rng> generator) const {
            Eigen::VectorXd samples(this->parameter_table.size());

            for (int i = 0; i < this->parameter_table.size(); i++) {
                double mean = this->parameter_table[i].mean->get_assignment();
                double variance =
                    this->parameter_table[i].variance->get_assignment();
                double sample =
                    mean + gsl_ran_gaussian(generator.get(), variance);
                samples(i) = sample;
            }

            return samples;
        }

        void GaussianCPD::print(std::ostream& os) const {
            os << "Gaussian CPD: {\n";
            for (auto& parameters : this->parameter_table) {
                os << " Gaussian(" << *(parameters.mean) << ","
                   << *(parameters.variance) << ")"
                   << "\n";
            }
            os << "}";
        }

    } // namespace model
} // namespace tomcat