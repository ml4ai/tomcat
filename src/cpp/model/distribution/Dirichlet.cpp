#include "Dirichlet.h"

#include <gsl/gsl_randist.h>

#include "model/pgm/ConstantNode.h"

namespace tomcat {
    namespace model {

        using namespace std;

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        Dirichlet::Dirichlet(vector<shared_ptr<Node>>& alpha)
            : Continuous(alpha) {}

        Dirichlet::Dirichlet(vector<shared_ptr<Node>>&& alpha)
            : Continuous(move(alpha)) {}

        Dirichlet::Dirichlet(const Eigen::VectorXd& alpha) {
            for (int i = 0; i < alpha.size(); i++) {
                ConstantNode parameter_node(alpha(i));
                this->parameters.push_back(
                    make_shared<ConstantNode>(move(parameter_node)));
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
        Dirichlet::sample(shared_ptr<gsl_rng> random_generator,
                          int parameter_idx) const {
            Eigen::VectorXd alpha = this->get_alpha(parameter_idx);

            return this->sample_from_gsl(random_generator, alpha);
        }

        Eigen::VectorXd Dirichlet::get_alpha(int parameter_idx) const {
            Eigen::VectorXd alpha(this->parameters.size());

            for (int i = 0; i < alpha.size(); i++) {
                Eigen::MatrixXd alphas = this->parameters[i]->get_assignment();
                parameter_idx = alphas.rows() == 1 ? 0 : parameter_idx;

                alpha(i) = alphas(parameter_idx, 0);
            }

            return alpha;
        }

        Eigen::VectorXd
        Dirichlet::sample_from_gsl(shared_ptr<gsl_rng> random_generator,
                                   const Eigen::VectorXd& parameters) const {
            int k = this->parameters.size();
            double* sample_ptr = new double[k];

            const double* alpha = parameters.data();

            gsl_ran_dirichlet(random_generator.get(), k, alpha, sample_ptr);

            Eigen::Map<Eigen::VectorXd> sample(sample_ptr, k);

            return sample;
        }

        Eigen::VectorXd
        Dirichlet::sample(shared_ptr<gsl_rng> random_generator,
                          int parameter_idx,
                          const Eigen::VectorXd& weights) const {
            Eigen::VectorXd alpha = this->get_alpha(parameter_idx) * weights;

            return this->sample_from_gsl(random_generator, alpha);
        }

        Eigen::VectorXd Dirichlet::sample_from_conjugacy(
            shared_ptr<gsl_rng> random_generator,
            int parameter_idx,
            const Eigen::VectorXd& sufficient_statistics) const {

            Eigen::VectorXd alpha = this->get_alpha(parameter_idx);
            alpha = alpha + sufficient_statistics;

            return this->sample_from_gsl(random_generator, alpha);
        }

        double Dirichlet::get_pdf(const Eigen::VectorXd& value,
                                  int parameter_idx) const {
            Eigen::VectorXd alpha = this->get_alpha(parameter_idx);
            int k = alpha.size();
            const double* alpha_ptr = alpha.data();
            const double* value_ptr = value.data();

            return gsl_ran_dirichlet_pdf(k, alpha_ptr, value_ptr);
        }

        unique_ptr<Distribution> Dirichlet::clone() const {
            unique_ptr<Dirichlet> new_distribution =
                make_unique<Dirichlet>(*this);

            for (auto& parameter : new_distribution->parameters) {
                parameter = parameter->clone();
            }

            return new_distribution;
        }

        string Dirichlet::get_description() const {
            stringstream ss;
            ss << "Dir\n";
            ss << "(\n";
            for (const auto& parameter : this->parameters) {
                ss << " " << parameter << "\n";
            }
            ss << ")";

            return ss.str();
        }

        int Dirichlet::get_sample_size() const {
            return this->parameters.size();
        }

    } // namespace model
} // namespace tomcat
