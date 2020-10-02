#include "pgm/cpd/CategoricalCPD.h"
#include "pgm/ConstantNode.h"

namespace tomcat {
    namespace model {

        using namespace std;

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        CategoricalCPD::CategoricalCPD(
            vector<shared_ptr<NodeMetadata>>& parent_node_order,
            const vector<shared_ptr<Categorical>>& distributions)
            : CPD(parent_node_order) {
            this->init_from_distributions(distributions);
        }

        CategoricalCPD::CategoricalCPD(
            vector<shared_ptr<NodeMetadata>>&& parent_node_order,
            const vector<shared_ptr<Categorical>>& distributions)
            : CPD(parent_node_order) {
            this->init_from_distributions(distributions);
        }

        CategoricalCPD::CategoricalCPD(
            vector<shared_ptr<NodeMetadata>>& parent_node_order,
            const Eigen::MatrixXd& probabilities)
            : CPD(parent_node_order) {
            this->init_from_matrix(probabilities);
        }

        CategoricalCPD::CategoricalCPD(
            vector<shared_ptr<NodeMetadata>>&& parent_node_order,
            const Eigen::MatrixXd& cpd_table)
            : CPD(parent_node_order) {
            this->init_from_matrix(cpd_table);
        }

        CategoricalCPD::~CategoricalCPD() {}

        //----------------------------------------------------------------------
        // Copy & Move constructors/assignments
        //----------------------------------------------------------------------
        CategoricalCPD::CategoricalCPD(const CategoricalCPD& cpd) {
            this->copy_cpd(cpd);
        }

        CategoricalCPD& CategoricalCPD::operator=(const CategoricalCPD& cpd) {
            this->copy_cpd(cpd);
            return *this;
        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void CategoricalCPD::init_from_distributions(
            const vector<shared_ptr<Categorical>>& distributions) {
            this->distributions.reserve(distributions.size());
            for (const auto& distribution : distributions) {
                this->distributions.push_back(distribution);
            }
        }

        void CategoricalCPD::init_from_matrix(const Eigen::MatrixXd& matrix) {
            for (int i = 0; i < matrix.rows(); i++) {
                shared_ptr<Categorical> distribution_ptr =
                    make_shared<Categorical>(
                        Categorical(move(matrix.row(i))));
                this->distributions.push_back(distribution_ptr);
            }
        }

        unique_ptr<CPD> CategoricalCPD::clone() const {
            unique_ptr<CategoricalCPD> new_cpd =
                make_unique<CategoricalCPD>(*this);
            new_cpd->clone_distributions();
            return new_cpd;
        }

        void CategoricalCPD::clone_distributions() {
            for (auto& distribution : this->distributions) {
                shared_ptr<Distribution> temp = distribution->clone();
                distribution = dynamic_pointer_cast<Categorical>(temp);
            }
        }

        string CategoricalCPD::get_description() const {
            stringstream ss;
            ss << "Categorical CPD: {\n";
            for (auto& probabilities : this->distributions) {
                ss << " " << *probabilities << "\n";
            }
            ss << "}";

            return ss.str();
        }

        void CategoricalCPD::add_to_sufficient_statistics(
            const Eigen::VectorXd& sample) {
            throw invalid_argument(
                "No conjugate prior with a categorical distribution.");
        }

        Eigen::MatrixXd CategoricalCPD::sample_from_conjugacy(
            shared_ptr<gsl_rng> random_generator,
            const vector<shared_ptr<Node>>& parent_nodes,
            int num_samples) const {
            throw invalid_argument(
                "No conjugate prior with a categorical distribution.");
        }

        void CategoricalCPD::reset_sufficient_statistics() {
            // Nothing to reset
        }

    } // namespace model
} // namespace tomcat
