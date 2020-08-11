#include "Gaussian.h"

#include <gsl/gsl_randist.h>

#include "../ConstantNode.h"

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
        Gaussian::sample(std::shared_ptr<gsl_rng> random_generator) const {
            double mean =
                this->parameters[PARAMETER_INDEX::mean]->get_assignment()(0);
            double variance =
                this->parameters[PARAMETER_INDEX::mean]->get_assignment()(0);

            return this->sample_from_gsl(random_generator, mean, variance);
        }

        Eigen::VectorXd
        Gaussian::sample_from_gsl(std::shared_ptr<gsl_rng> random_generator,
                                  double mean,
                                  double variance) const {

            double sample =
                mean + gsl_ran_gaussian(random_generator.get(), variance);

            Eigen::VectorXd sample_vector(1);
            sample_vector(0) = sample;

            return sample_vector;
        }

        Eigen::VectorXd
        Gaussian::sample(std::shared_ptr<gsl_rng> random_generator,
                         Eigen::VectorXd weights) const {

            // In a Gaussian distribution the weights are the new parameters.

            double mean = weights(PARAMETER_INDEX::mean);
            double variance = weights(PARAMETER_INDEX::variance);

            return this->sample_from_gsl(random_generator, mean, variance);
        }

        double Gaussian::get_pdf(Eigen::VectorXd value) const {
            double mean =
                this->parameters[PARAMETER_INDEX::mean]->get_assignment()(0);
            double variance = sqrt(
                this->parameters[PARAMETER_INDEX::mean]->get_assignment()(0));

            double pdf = gsl_ran_gaussian_pdf(value(0) - mean, variance);

            return pdf;
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

    } // namespace model
} // namespace tomcat
