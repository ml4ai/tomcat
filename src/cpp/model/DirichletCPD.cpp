#include "DirichletCPD.h"

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
        DirichletCPD::DirichletCPD(
            std::vector<std::string>& parent_node_label_order,
            std::vector<std::shared_ptr<Node>>& parameter_table)
            : ContinuousCPD(parent_node_label_order) {

            this->init_from_table(parameter_table);
        }

        DirichletCPD::DirichletCPD(
            std::vector<std::string>&& parent_node_label_order,
            std::vector<std::shared_ptr<Node>>&& parameter_table)
            : ContinuousCPD(std::move(parent_node_label_order)) {

            this->init_from_table(parameter_table);
        }

        DirichletCPD::DirichletCPD(
            std::vector<std::string>& parent_node_label_order,
            const Eigen::MatrixXd& parameter_values)
            : ContinuousCPD(parent_node_label_order) {

            this->init_from_matrix(parameter_values);
        }

        DirichletCPD::DirichletCPD(
            std::vector<std::string>&& parent_node_label_order,
            const Eigen::MatrixXd&& parameter_values)
            : ContinuousCPD(std::move(parent_node_label_order)) {

            this->init_from_matrix(parameter_values);
        }

        DirichletCPD::~DirichletCPD() {}

        //----------------------------------------------------------------------
        // Copy & Move constructors/assignments
        //----------------------------------------------------------------------
        DirichletCPD::DirichletCPD(const DirichletCPD& cpd) {
            this->copy_from_cpd(cpd);
        }

        DirichletCPD& DirichletCPD::operator=(const DirichletCPD& cpd) {
            this->copy_from_cpd(cpd);
            return *this;
        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void DirichletCPD::init_from_table(
            std::vector<std::shared_ptr<Node>>& parameter_table) {
            // Each node contains the parameter vector alpha.
            for (int i = 0; i < parameter_table.size(); i++) {
                std::vector<std::shared_ptr<Node>> parameters;
                parameters.push_back(parameter_table[i]);
                this->parameter_table.push_back(std::move(parameters));
            }
        }

        void DirichletCPD::init_from_matrix(const Eigen::MatrixXd& matrix) {
            for (int row = 0; row < matrix.rows(); row++) {
                Eigen::VectorXd alpha = matrix.row(row);
                std::vector<std::shared_ptr<Node>> alpha_vector;
                alpha_vector.push_back(
                    std::make_shared<ConstantNode>(ConstantNode(alpha)));
                this->parameter_table.push_back(std::move(alpha_vector));
            }
        }

        Eigen::VectorXd DirichletCPD::sample_from_table_row(
            std::shared_ptr<gsl_rng> random_generator, int table_row) const {
            int alpha_size = this->parameter_table[table_row][0]
                                 ->get_metadata()
                                 ->get_sample_size();
            double* sample_ptr = new double[alpha_size];

            const double* alpha =
                this->parameter_table[table_row][0]->get_assignment().data();

            gsl_ran_dirichlet(
                random_generator.get(), alpha_size, alpha, sample_ptr);

            Eigen::Map<Eigen::VectorXd> sample(sample_ptr, alpha_size);

            return sample;
        }

        std::unique_ptr<CPD> DirichletCPD::clone() const {
            return std::make_unique<DirichletCPD>(*this);
        }

        std::shared_ptr<CPD> DirichletCPD::clone_shared() const {
            return std::make_shared<DirichletCPD>(*this);
        }

        std::string DirichletCPD::get_description() const {
            std::stringstream ss;

            ss << "Dirichlet CPD: {\n";
            for (auto& alpha : this->parameter_table) {
                ss << " " << *alpha[0] << "\n";
            }
            ss << "}";

            return ss.str();
        }

    } // namespace model
} // namespace tomcat