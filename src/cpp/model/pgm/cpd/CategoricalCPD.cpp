#include "CategoricalCPD.h"

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
        CategoricalCPD::CategoricalCPD(
            std::vector<std::shared_ptr<NodeMetadata>>& parent_node_order,
            const std::vector<std::shared_ptr<Categorical>>& distributions)
            : CPD(parent_node_order) {
            this->init_from_distributions(distributions);
        }

        CategoricalCPD::CategoricalCPD(
            std::vector<std::shared_ptr<NodeMetadata>>&& parent_node_order,
            const std::vector<std::shared_ptr<Categorical>>& distributions)
            : CPD(parent_node_order) {
            this->init_from_distributions(distributions);
        }

        CategoricalCPD::CategoricalCPD(
            std::vector<std::shared_ptr<NodeMetadata>>& parent_node_order,
            const Eigen::MatrixXd& probabilities)
            : CPD(parent_node_order) {
            this->init_from_matrix(probabilities);
        }

        CategoricalCPD::CategoricalCPD(
            std::vector<std::shared_ptr<NodeMetadata>>&& parent_node_order,
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
            const std::vector<std::shared_ptr<Categorical>>& distributions) {
            this->distributions.reserve(distributions.size());
            for (const auto& distribution : distributions) {
                this->distributions.push_back(distribution);
            }
        }

        void CategoricalCPD::init_from_matrix(const Eigen::MatrixXd& matrix) {
            for (int i = 0; i < matrix.rows(); i++) {
                std::shared_ptr<Categorical> distribution_ptr =
                    std::make_shared<Categorical>(
                        Categorical(std::move(matrix.row(i))));
                this->distributions.push_back(distribution_ptr);
            }
        }

        std::unique_ptr<CPD> CategoricalCPD::clone() const {
            std::unique_ptr<CategoricalCPD> new_cpd =
                std::make_unique<CategoricalCPD>(*this);
            new_cpd->clone_distributions();
            return new_cpd;
        }

        void CategoricalCPD::clone_distributions() {
            for (auto& distribution : this->distributions) {
                std::shared_ptr<Distribution> temp = distribution->clone();
                distribution = std::dynamic_pointer_cast<Categorical>(temp);
            }
        }

        std::string CategoricalCPD::get_description() const {
            std::stringstream ss;
            ss << "Categorical CPD: {\n";
            for (auto& probabilities : this->distributions) {
                ss << " " << *probabilities << "\n";
            }
            ss << "}";

            return ss.str();
        }

        void CategoricalCPD::add_to_sufficient_statistics(
            const Eigen::VectorXd& sample) {
            throw std::invalid_argument(
                "No conjugate prior with a categorical distribution.");
        }

        Eigen::MatrixXd CategoricalCPD::sample_from_conjugacy(
            std::shared_ptr<gsl_rng> random_generator,
            const std::vector<std::shared_ptr<Node>>& parent_nodes,
            int num_samples) const {
            throw std::invalid_argument(
                "No conjugate prior with a categorical distribution.");
        }

        void CategoricalCPD::reset_sufficient_statistics() {
            // Nothing to reset
        }

    } // namespace model
} // namespace tomcat