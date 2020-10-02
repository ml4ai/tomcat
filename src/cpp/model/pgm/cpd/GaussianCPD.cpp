#include "pgm/cpd/GaussianCPD.h"
#include "pgm/ConstantNode.h"

namespace tomcat {
    namespace model {

        using namespace std;

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        GaussianCPD::GaussianCPD(
            vector<shared_ptr<NodeMetadata>>& parent_node_order,
            vector<shared_ptr<Gaussian>>& distributions)
            : CPD(parent_node_order) {

            this->distributions.reserve(distributions.size());
            for (const auto& distribution : distributions) {
                this->distributions.push_back(distribution);
            }
        }

        GaussianCPD::GaussianCPD(
            vector<shared_ptr<NodeMetadata>>&& parent_node_order,
            vector<shared_ptr<Gaussian>>&& distributions)
            : CPD(parent_node_order) {

            this->distributions.reserve(distributions.size());
            for (const auto& distribution : distributions) {
                this->distributions.push_back(distribution);
            }
        }

        GaussianCPD::GaussianCPD(
            vector<shared_ptr<NodeMetadata>>& parent_node_order,
            const Eigen::MatrixXd& parameters)
            : CPD(parent_node_order) {
            this->init_from_matrix(parameters);
        }

        GaussianCPD::GaussianCPD(
            vector<shared_ptr<NodeMetadata>>&& parent_node_order,
            const Eigen::MatrixXd& parameters)
            : CPD(parent_node_order) {
            this->init_from_matrix(parameters);
        }

        GaussianCPD::~GaussianCPD() {}

        //----------------------------------------------------------------------
        // Copy & Move constructors/assignments
        //----------------------------------------------------------------------
        GaussianCPD::GaussianCPD(const GaussianCPD& cpd) {
            this->copy_cpd(cpd);
        }

        GaussianCPD& GaussianCPD::operator=(const GaussianCPD& cpd) {
            this->copy_cpd(cpd);
            return *this;
        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void GaussianCPD::init_from_matrix(const Eigen::MatrixXd& matrix) {
            for (int row = 0; row < matrix.rows(); row++) {
                for (int i = 0; i < matrix.rows(); i++) {
                    shared_ptr<Gaussian> distribution_ptr =
                        make_shared<Gaussian>(Gaussian(matrix.row(i)));
                    this->distributions.push_back(distribution_ptr);
                }
            }
        }

        unique_ptr<CPD> GaussianCPD::clone() const {
            unique_ptr<GaussianCPD> new_cpd =
                make_unique<GaussianCPD>(*this);
            new_cpd->clone_distributions();
            return new_cpd;
        }

        void GaussianCPD::clone_distributions() {
            for (auto& distribution : this->distributions) {
                shared_ptr<Distribution> temp = distribution->clone();
                distribution = dynamic_pointer_cast<Gaussian>(temp);
            }
        }

        string GaussianCPD::get_description() const {
            stringstream ss;

            ss << "Gaussian CPD: {\n";
            for (auto& parameters : this->distributions) {
                ss << *parameters << "\n";
            }
            ss << "}";

            return ss.str();
        }

        void GaussianCPD::add_to_sufficient_statistics(
            const Eigen::VectorXd& sample) {
            throw invalid_argument(
                "Not implemented yet.");
        }

        Eigen::MatrixXd GaussianCPD::sample_from_conjugacy(
            shared_ptr<gsl_rng> random_generator,
            const vector<shared_ptr<Node>>& parent_nodes,
            int num_samples) const {
            throw invalid_argument(
                "Not implemented yet.");
        }

        void GaussianCPD::reset_sufficient_statistics() {
            throw invalid_argument(
                "Not implemented yet.");
        }

    } // namespace model
} // namespace tomcat
