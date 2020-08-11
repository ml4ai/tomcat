#include "GaussianCPD.h"

#include "ConstantNode.h"

namespace tomcat {
    namespace model {
        //----------------------------------------------------------------------
        // Definitions
        //----------------------------------------------------------------------

        // No definitions in this file

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        GaussianCPD::GaussianCPD(
            std::vector<std::shared_ptr<NodeMetadata>>& parent_node_order,
            std::vector<std::shared_ptr<Gaussian>>& distributions)
            : CPD(parent_node_order) {

            this->distributions.reserve(distributions.size());
            for (const auto& distribution : distributions) {
                this->distributions.push_back(distribution);
            }
        }

        GaussianCPD::GaussianCPD(
            std::vector<std::shared_ptr<NodeMetadata>>&& parent_node_order,
            std::vector<std::shared_ptr<Gaussian>>&& distributions)
            : CPD(parent_node_order) {

            this->distributions.reserve(distributions.size());
            for (const auto& distribution : distributions) {
                this->distributions.push_back(distribution);
            }
        }

        GaussianCPD::GaussianCPD(
            std::vector<std::shared_ptr<NodeMetadata>>& parent_node_order,
            const Eigen::MatrixXd& parameters)
            : CPD(parent_node_order) {
            this->init_from_matrix(parameters);
        }

        GaussianCPD::GaussianCPD(
            std::vector<std::shared_ptr<NodeMetadata>>&& parent_node_order,
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
                    std::shared_ptr<Gaussian> distribution_ptr =
                        std::make_shared<Gaussian>(Gaussian(matrix.row(i)));
                    this->distributions.push_back(distribution_ptr);
                }
            }
        }

        std::unique_ptr<CPD> GaussianCPD::clone() const {
            std::unique_ptr<GaussianCPD> new_cpd =
                std::make_unique<GaussianCPD>(*this);
            new_cpd->clone_distributions();
            return new_cpd;
        }

        void GaussianCPD::clone_distributions() {
            for (auto& distribution : this->distributions) {
                std::shared_ptr<Distribution> temp = distribution->clone();
                distribution = std::dynamic_pointer_cast<Gaussian>(temp);
            }
        }

        std::string GaussianCPD::get_description() const {
            std::stringstream ss;

            ss << "Gaussian CPD: {\n";
            for (auto& parameters : this->distributions) {
                ss << *parameters << "\n";
            }
            ss << "}";

            return ss.str();
        }

    } // namespace model
} // namespace tomcat