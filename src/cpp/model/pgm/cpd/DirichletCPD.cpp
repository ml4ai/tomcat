#include "DirichletCPD.h"

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
        DirichletCPD::DirichletCPD(
            std::vector<std::shared_ptr<NodeMetadata>>& parent_node_order,
            const std::vector<std::shared_ptr<Dirichlet>>& distributions)
            : CPD(parent_node_order) {

            this->init_from_distributions(distributions);
        }

        DirichletCPD::DirichletCPD(
            std::vector<std::shared_ptr<NodeMetadata>>&& parent_node_order,
            const std::vector<std::shared_ptr<Dirichlet>>& distributions)
            : CPD(parent_node_order) {

            this->init_from_distributions(distributions);
        }

        DirichletCPD::DirichletCPD(
            std::vector<std::shared_ptr<NodeMetadata>>& parent_node_order,
            const Eigen::MatrixXd& alphas)
            : CPD(parent_node_order) {
            this->init_from_matrix(alphas);
        }

        DirichletCPD::DirichletCPD(
            std::vector<std::shared_ptr<NodeMetadata>>&& parent_node_order,
            const Eigen::MatrixXd& alphas)
            : CPD(parent_node_order) {
            this->init_from_matrix(alphas);
        }

        DirichletCPD::~DirichletCPD() {}

        //----------------------------------------------------------------------
        // Copy & Move constructors/assignments
        //----------------------------------------------------------------------
        DirichletCPD::DirichletCPD(const DirichletCPD& cpd) {
            this->copy_cpd(cpd);
            this->sufficient_statistics = cpd.sufficient_statistics;
        }

        DirichletCPD& DirichletCPD::operator=(const DirichletCPD& cpd) {
            this->copy_cpd(cpd);
            this->sufficient_statistics = cpd.sufficient_statistics;
            return *this;
        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void DirichletCPD::init_from_distributions(
            const std::vector<std::shared_ptr<Dirichlet>>& distributions) {
            this->distributions.reserve(distributions.size());
            int sample_size = 0;
            for (const auto& distribution : distributions) {
                this->distributions.push_back(distribution);
                sample_size = distribution->get_sample_size();
            }
            this->sufficient_statistics = Eigen::VectorXd::Zero(sample_size);
            this->sufficient_statistics = Eigen::VectorXd(sample_size);
        }

        void DirichletCPD::init_from_matrix(const Eigen::MatrixXd& matrix) {
            for (int i = 0; i < matrix.rows(); i++) {
                std::shared_ptr<Dirichlet> distribution_ptr =
                    std::make_shared<Dirichlet>(Dirichlet(matrix.row(i)));
                this->distributions.push_back(distribution_ptr);
            }
            this->sufficient_statistics = Eigen::VectorXd::Zero(matrix.cols());
        }

        std::unique_ptr<CPD> DirichletCPD::clone() const {
            std::unique_ptr<DirichletCPD> new_cpd =
                std::make_unique<DirichletCPD>(*this);
            new_cpd->clone_distributions();
            new_cpd->sufficient_statistics = this->sufficient_statistics;
            return new_cpd;
        }

        void DirichletCPD::clone_distributions() {
            for (auto& distribution : this->distributions) {
                std::shared_ptr<Distribution> temp = distribution->clone();
                distribution = std::dynamic_pointer_cast<Dirichlet>(temp);
            }
        }

        std::string DirichletCPD::get_description() const {
            std::stringstream ss;

            ss << "Dirichlet CPD: {\n";
            for (auto& alpha : this->distributions) {
                ss << " " << *alpha << "\n";
            }
            ss << "}";

            return ss.str();
        }

        void DirichletCPD::add_to_sufficient_statistics(
            const Eigen::VectorXd& sample) {
            this->sufficient_statistics(sample(0)) += 1;
        }

        Eigen::MatrixXd DirichletCPD::sample_from_conjugacy(
            std::shared_ptr<gsl_rng> random_generator,
            const std::vector<std::shared_ptr<Node>>& parent_nodes,
            int num_samples) const {

            std::vector<int> distribution_indices =
                this->get_distribution_indices(parent_nodes, num_samples);

            int sample_size = this->distributions[0]->get_sample_size();

            Eigen::MatrixXd samples(distribution_indices.size(), sample_size);
            int i = 0;
            for (const auto& distribution_idx : distribution_indices) {
                Eigen::VectorXd assignment =
                    this->distributions[distribution_idx]
                        ->sample_from_conjugacy(random_generator,
                                                distribution_idx,
                                                this->sufficient_statistics);
                samples.row(i) = std::move(assignment);
                i++;
            }

            return samples;
        }

        void DirichletCPD::reset_sufficient_statistics() {
            this->sufficient_statistics = Eigen::VectorXd::Zero(this->sufficient_statistics.size());
        }

    } // namespace model
} // namespace tomcat