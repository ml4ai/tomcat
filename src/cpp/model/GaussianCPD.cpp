#include "GaussianCPD.h"
#include "ConstantNode.h"

namespace tomcat {
    namespace model {
        void GaussianCPD::init_from_table(
            std::vector<GaussianParameters>& parameter_table) {

            for (int i = 0; i < parameter_table.size(); i++) {
                std::vector<std::shared_ptr<Node>> parameters(2);
                parameters.push_back(std::move(parameter_table[i].mean));
                parameters.push_back(std::move(parameter_table[i].variance));
                this->parameter_table.push_back(std::move(parameters));
            }
        }

        void GaussianCPD::init_from_matrix(const Eigen::MatrixXd& matrix) {
            for (int row = 0; row < matrix.rows(); row++) {
                double mean = matrix(row, PARAMETER_INDEX::mean);
                double variance = matrix(row, PARAMETER_INDEX::variance);

                std::vector<std::shared_ptr<Node>> parameters(2);
                parameters.push_back(
                    std::make_shared<ConstantNode>(ConstantNode(mean)));
                parameters.push_back(
                    std::make_shared<ConstantNode>(ConstantNode(variance)));
                this->parameter_table.push_back(std::move(parameters));
            }
        }

        Eigen::MatrixXd
        GaussianCPD::sample(std::shared_ptr<gsl_rng> random_generator) const {
            Eigen::VectorXd samples(this->parameter_table.size());

            for (int i = 0; i < this->parameter_table.size(); i++) {
                samples(i) = this->sample(random_generator, i)(0);
            }

            return samples;
        }

        Eigen::VectorXd
        GaussianCPD::sample(std::shared_ptr<gsl_rng> random_generator,
                            int index) const {
            Eigen::VectorXd sample_vector(1);

            double mean = this->parameter_table[index][PARAMETER_INDEX::mean]
                              ->get_assignment()(0);
            double variance =
                this->parameter_table[index][PARAMETER_INDEX::variance]
                    ->get_assignment()(0);
            double sample =
                mean + gsl_ran_gaussian(random_generator.get(), variance);
            sample_vector(0) = sample;

            return sample_vector;
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

        std::shared_ptr<CPD> GaussianCPD::clone_shared() const {
            return std::make_shared<GaussianCPD>(*this);
        }

    } // namespace model
} // namespace tomcat