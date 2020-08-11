#include "Dirichlet.h"

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
        Dirichlet::Dirichlet(std::vector<std::shared_ptr<Node>>& alpha)
            : Continuous(alpha) {}

        Dirichlet::Dirichlet(std::vector<std::shared_ptr<Node>>&& alpha)
            : Continuous(std::move(alpha)) {}

        Dirichlet::Dirichlet(const Eigen::VectorXd& alpha) {
            for (int i = 0; i < alpha.size(); i++) {
                ConstantNode parameter_node(alpha(i));
                this->parameters.push_back(
                    std::make_shared<ConstantNode>(std::move(parameter_node)));
            }
        }

        Dirichlet::~Dirichlet() {}

        //----------------------------------------------------------------------
        // Copy & Move constructors/assignments
        //----------------------------------------------------------------------
        Dirichlet::Dirichlet(const Dirichlet& dirichlet) {
            this->parameters = dirichlet.parameters;
        }

        Dirichlet& Dirichlet::operator=(const Dirichlet& dirichlet) {
            this->parameters = dirichlet.parameters;
            return *this;
        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        Eigen::VectorXd
        Dirichlet::sample(std::shared_ptr<gsl_rng> random_generator) const {
            return this->sample_from_gsl(random_generator, this->get_alpha());
        }

        Eigen::VectorXd Dirichlet::get_alpha() const{
            Eigen::VectorXd alpha(this->parameters.size());

            int i = 0;
            for (const auto& parameter_node : this->parameters) {
                alpha(i) = parameter_node->get_assignment()(0);
                i++;
            }

            return alpha;
        }

        Eigen::VectorXd
        Dirichlet::sample_from_gsl(std::shared_ptr<gsl_rng> random_generator,
                                   const Eigen::VectorXd& parameters) const {
            int k = this->parameters.size();
            double* sample_ptr = new double[k];

            const double* alpha = parameters.data();

            gsl_ran_dirichlet(
                random_generator.get(), k, alpha, sample_ptr);

            Eigen::Map<Eigen::VectorXd> sample(sample_ptr, k);

            return sample;
        }

        Eigen::VectorXd
        Dirichlet::sample(std::shared_ptr<gsl_rng> random_generator,
                          Eigen::VectorXd weights) const {

            Eigen::VectorXd weighted_params(this->parameters.size());

            int i = 0;
            for (const auto& parameter_node : this->parameters) {
                // In a Dirichlet distribution the weights are sufficient
                // statistics added to each one of the coefficients.
                weighted_params(i) =
                    parameter_node->get_assignment()(0) + weights(i);
            }

            return this->sample_from_gsl(random_generator, weighted_params);
        }

        double Dirichlet::get_pdf(Eigen::VectorXd value) const {
            Eigen::VectorXd alpha = this->get_alpha();
            int k = alpha.size();
            const double* params_ptr = alpha.data();
            const double* sample_ptr = value.data();

            double pdf = gsl_ran_dirichlet_pdf(k, params_ptr, sample_ptr);

            return pdf;
        }

        std::unique_ptr<Distribution> Dirichlet::clone() const {
            std::unique_ptr<Dirichlet> new_distribution =
                std::make_unique<Dirichlet>(*this);

            for(auto& parameter : new_distribution->parameters){
                parameter = parameter->clone();
            }

            return new_distribution;
        }

        std::string Dirichlet::get_description() const {
            std::stringstream ss;
            ss << "Dir\n";
            ss << "(\n";
            for(const auto& parameter : this->parameters) {
                ss << " " << parameter << "\n";
            }
            ss << ")";


            return ss.str();
        }

    } // namespace model
} // namespace tomcat
