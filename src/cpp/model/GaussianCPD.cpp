#include "GaussianCPD.h"

namespace tomcat {
    namespace model {

        GaussianCPD::GaussianCPD(
            std::vector<std::string> parent_node_label_order,
            std::vector<GaussianParameters> parameter_table)
            : ContinuousCPD(std::move(parent_node_label_order)) {

            for (int i = 0; i < parameter_table.size(); i++) {
                std::vector<std::unique_ptr<Node>> parameters;
                parameters.reserve(2);
                parameters.push_back(
                    std::move(parameter_table[i].mean->clone()));
                parameters.push_back(
                    std::move(parameter_table[i].variance->clone()));
                this->parameter_table.push_back(std::move(parameters));
            }
        }

        GaussianCPD::GaussianCPD(
            std::vector<std::string> parent_node_label_order,
            Eigen::MatrixXd& parameter_values)
            : ContinuousCPD(std::move(parent_node_label_order)) {

            this->init_from_matrix(parameter_values);
        }

        void GaussianCPD::init_from_matrix(Eigen::MatrixXd& parameter_values) {
            for (int row = 0; row < parameter_values.rows(); row++) {
                double mean = parameter_values(row, PARAMETER_INDEX::mean);
                double variance =
                    parameter_values(row, PARAMETER_INDEX::variance);

                std::vector<std::unique_ptr<Node>> parameters;
                parameters.reserve(2);
                parameters.push_back(
                    std::move(std::make_unique<Node>(Node(mean))));
                parameters.push_back(
                    std::move(std::make_unique<Node>(Node(variance))));
                this->parameter_table.push_back(std::move(parameters));
            }
        }

        Eigen::MatrixXd
        GaussianCPD::sample(std::shared_ptr<gsl_rng> generator) const {
            Eigen::VectorXd samples(this->parameter_table.size());

            for (int i = 0; i < this->parameter_table.size(); i++) {
                double mean = this->parameter_table[i][0]->get_assignment()(0);
                double variance =
                    this->parameter_table[i][1]->get_assignment()(0);
                double sample =
                    mean + gsl_ran_gaussian(generator.get(), variance);
                samples(i) = sample;
            }

            return samples;
        }

        void GaussianCPD::print(std::ostream& os) const {
            os << "Gaussian CPD: {\n";
            for (auto& parameters : this->parameter_table) {
                os << " Gaussian(" << *parameters[PARAMETER_INDEX::mean] << ","
                   << *parameters[PARAMETER_INDEX::variance] << ")"
                   << "\n";
            }
            os << "}";
        }

        std::unique_ptr<CPD> GaussianCPD::clone() const {
            return std::make_unique<GaussianCPD>(*this);
        }

    } // namespace model
} // namespace tomcat