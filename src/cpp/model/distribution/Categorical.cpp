#include "Categorical.h"

#include <gsl/gsl_randist.h>

#include "model/pgm/ConstantNode.h"
#include "model/pgm/RandomVariableNode.h"

namespace tomcat {
    namespace model {

        using namespace std;

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        Categorical::Categorical(shared_ptr<Node>& probabilities)
            : probabilities(probabilities) {}

        Categorical::Categorical(shared_ptr<Node>&& probabilities)
            : probabilities(move(probabilities)) {}

        Categorical::Categorical(const Eigen::VectorXd& probabilities) {
            this->probabilities =
                make_shared<ConstantNode>(ConstantNode(probabilities));
        }

        Categorical::Categorical(const Eigen::VectorXd&& probabilities) {
            this->probabilities = make_shared<ConstantNode>(
                ConstantNode(move(probabilities)));
        }

        Categorical::~Categorical() {}

        //----------------------------------------------------------------------
        // Copy & Move constructors/assignments
        //----------------------------------------------------------------------
        Categorical::Categorical(const Categorical& categorical) {
            this->probabilities = categorical.probabilities;
        }

        Categorical& Categorical::operator=(const Categorical& categorical) {
            this->probabilities = categorical.probabilities;
            return *this;
        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void
        Categorical::update_dependencies(Node::NodeMap& parameter_nodes_map,
                                         int time_step) {

            string parameter_timed_name;
            const NodeMetadata* metadata =
                this->probabilities->get_metadata().get();
            if (metadata->is_replicable()) {
                parameter_timed_name = metadata->get_timed_name(time_step);
            }
            else {
                parameter_timed_name =
                    metadata->get_timed_name(metadata->get_initial_time_step());
            }

            if (parameter_nodes_map.count(parameter_timed_name) > 0) {
                this->probabilities = parameter_nodes_map[parameter_timed_name];
            }
        }

        Eigen::VectorXd
        Categorical::sample(shared_ptr<gsl_rng> random_generator,
                            int parameter_idx) const {
            Eigen::MatrixXd probabilities =
                this->probabilities->get_assignment();

            parameter_idx = probabilities.rows() == 1 ? 0 : parameter_idx;

            return this->sample_from_gsl(random_generator,
                                         probabilities.row(parameter_idx));
        }

        Eigen::VectorXd
        Categorical::sample_from_gsl(shared_ptr<gsl_rng> random_generator,
                                     const Eigen::VectorXd& parameters) const {

            Eigen::VectorXd checked_parameters;
            // If for some reason, all the probabilities are zero, sample from a
            // uniform distribution;
            if (parameters.sum() < EPSILON) {
                checked_parameters = Eigen::VectorXd::Ones(parameters.size());
            }
            else {
                checked_parameters = parameters;
            }

            int k = parameters.size();
            const double* parameters_ptr = checked_parameters.data();
            unsigned int* sample_ptr = new unsigned int[k];

            gsl_ran_multinomial(
                random_generator.get(), k, 1, parameters_ptr, sample_ptr);

            Eigen::VectorXd sample_vector(1);
            sample_vector(0) = this->get_sample_index(sample_ptr, k);

            delete[] sample_ptr;

            return sample_vector;
        }

        unsigned int
        Categorical::get_sample_index(const unsigned int* sample_array,
                                      size_t array_size) const {
            return distance(
                sample_array,
                find(sample_array, sample_array + array_size, 1));
        }

        Eigen::VectorXd
        Categorical::sample(shared_ptr<gsl_rng> random_generator,
                            int parameter_idx,
                            const Eigen::VectorXd& weights) const {

            Eigen::MatrixXd probabilities =
                this->probabilities->get_assignment();

            parameter_idx = probabilities.rows() == 1 ? 0 : parameter_idx;

            Eigen::VectorXd weighted_probabilities =
                probabilities.row(parameter_idx).transpose().array() *
                weights.array();

            // weighted_probs does not need to be normalized because GSL already
            // does that.
            return this->sample_from_gsl(random_generator,
                                         weighted_probabilities);
        }

        Eigen::VectorXd Categorical::sample_from_conjugacy(
            shared_ptr<gsl_rng> random_generator,
            int parameter_idx,
            const Eigen::VectorXd& sufficient_statistics) const {
            throw invalid_argument(
                "No conjugate prior with a categorical distribution.");
        }

        double Categorical::get_pdf(const Eigen::VectorXd& value,
                                    int parameter_idx) const {
            Eigen::MatrixXd probabilities =
                this->probabilities->get_assignment();
            parameter_idx = probabilities.rows() == 1 ? 0 : parameter_idx;
            return probabilities(parameter_idx, value(0));
        }

        unique_ptr<Distribution> Categorical::clone() const {
            unique_ptr<Categorical> new_distribution =
                make_unique<Categorical>(*this);
            new_distribution->probabilities =
                new_distribution->probabilities->clone();

            return new_distribution;
        }

        string Categorical::get_description() const {
            stringstream ss;
            ss << "Cat(" << this->probabilities << ")";

            return ss.str();
        }

        int Categorical::get_sample_size() const { return 1; }

        void Categorical::update_sufficient_statistics(
            const Eigen::VectorXd& sample) {
            if (this->probabilities->get_metadata()->is_parameter()) {
                if (RandomVariableNode* rv_node =
                        dynamic_cast<RandomVariableNode*>(
                            this->probabilities.get())) {
                    rv_node->add_to_sufficient_statistics(sample);
                }
            }
        }

        Eigen::VectorXd Categorical::get_values() const {
            return this->probabilities->get_assignment().row(0);
        }

    } // namespace model
} // namespace tomcat
