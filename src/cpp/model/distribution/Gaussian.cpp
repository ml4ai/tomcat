#include "Gaussian.h"

#include <gsl/gsl_randist.h>

#include "../pgm/ConstantNode.h"

namespace tomcat {
    namespace model {

        //----------------------------------------------------------------------
        // Definitions
        //----------------------------------------------------------------------

        // No definitions in this file

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        Gaussian::Gaussian(std::shared_ptr<Node>& mean,
                           std::shared_ptr<Node>& variance)
            : Continuous({mean, variance}) {}

        Gaussian::Gaussian(std::shared_ptr<Node>&& mean,
                           std::shared_ptr<Node>&& variance)
            : Continuous({std::move(mean), std::move(variance)}) {}

        Gaussian::Gaussian(const Eigen::VectorXd& parameters) {
            ConstantNode mean(parameters[PARAMETER_INDEX::mean]);
            ConstantNode variance(parameters[PARAMETER_INDEX::variance]);

            this->parameters.push_back(
                std::make_shared<ConstantNode>(std::move(mean)));
            this->parameters.push_back(
                std::make_shared<ConstantNode>(std::move(variance)));
        }

        Gaussian::~Gaussian() {}

        //----------------------------------------------------------------------
        // Copy & Move constructors/assignments
        //----------------------------------------------------------------------
        Gaussian::Gaussian(const Gaussian& Gaussian) {
            this->parameters = Gaussian.parameters;
        }

        Gaussian& Gaussian::operator=(const Gaussian& Gaussian) {
            this->parameters = Gaussian.parameters;
            return *this;
        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        Eigen::VectorXd
        Gaussian::sample(std::shared_ptr<gsl_rng> random_generator,
                         int parameter_idx) const {

            Eigen::VectorXd parameters = this->get_parameters(parameter_idx);
            double mean = parameters(PARAMETER_INDEX::mean);
            double variance = parameters(PARAMETER_INDEX::variance);

            return this->sample_from_gsl(random_generator, mean, variance);
        }

        Eigen::VectorXd Gaussian::get_parameters(int parameter_idx) const {
            Eigen::VectorXd means =
                this->parameters[PARAMETER_INDEX::mean]->get_assignment().col(
                    0);
            Eigen::VectorXd variances =
                this->parameters[PARAMETER_INDEX::variance]
                    ->get_assignment()
                    .col(0);

            int mean_idx = means.size() == 1 ? 0 : parameter_idx;
            double mean = means(mean_idx);

            int variance_idx = variances.size() == 1 ? 0 : parameter_idx;
            double variance = variances(variance_idx);

            Eigen::VectorXd parameters(2);
            parameters(PARAMETER_INDEX::mean) = mean;
            parameters(PARAMETER_INDEX::variance) = variance;

            return parameters;
        }

        Eigen::VectorXd
        Gaussian::sample_from_gsl(std::shared_ptr<gsl_rng> random_generator,
                                  double mean,
                                  double variance) const {

            double sample =
                mean + gsl_ran_gaussian(random_generator.get(), sqrt(variance));

            Eigen::VectorXd sample_vector(1);
            sample_vector(0) = sample;

            return sample_vector;
        }

        Eigen::VectorXd
        Gaussian::sample(std::shared_ptr<gsl_rng> random_generator,
                         int parameter_idx,
                         const Eigen::VectorXd& weights) const {

            Eigen::VectorXd parameters =
                this->get_parameters(parameter_idx) * weights;
            double mean = parameters(PARAMETER_INDEX::mean);
            double variance = parameters(PARAMETER_INDEX::variance);

            return this->sample_from_gsl(random_generator, mean, variance);
        }

        Eigen::VectorXd Gaussian::sample_from_conjugacy(
            std::shared_ptr<gsl_rng> random_generator,
            int parameter_idx,
            const Eigen::VectorXd& sufficient_statistics) const {
            throw std::invalid_argument(
                "Not implemented yet.");
        }

        double Gaussian::get_pdf(const Eigen::VectorXd& value,
                                 int parameter_idx) const {

            Eigen::VectorXd parameters = this->get_parameters(parameter_idx);
            double mean = parameters(PARAMETER_INDEX::mean);
            double variance = parameters(PARAMETER_INDEX::variance);

            return gsl_ran_gaussian_pdf(value(0) - mean, sqrt(variance));
        }

        std::unique_ptr<Distribution> Gaussian::clone() const {
            std::unique_ptr<Gaussian> new_distribution =
                std::make_unique<Gaussian>(*this);

            for (auto& parameter : new_distribution->parameters) {
                parameter = parameter->clone();
            }

            return new_distribution;
        }

        std::string Gaussian::get_description() const {
            std::stringstream ss;
            ss << "N(" << this->parameters[0] << ", " << this->parameters[1]
               << ")";

            return ss.str();
        }

        int Gaussian::get_sample_size() const { return 1; }

    } // namespace model
} // namespace tomcat