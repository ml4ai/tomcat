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
            std::vector<std::string>& parent_node_label_order,
            std::vector<GaussianParameters>& parameter_table)
            : ContinuousCPD(parent_node_label_order) {

            this->init_from_table(parameter_table);
        }

        GaussianCPD::GaussianCPD(
            std::vector<std::string>&& parent_node_label_order,
            std::vector<GaussianParameters>&& parameter_table)
            : ContinuousCPD(std::move(parent_node_label_order)) {

            this->init_from_table(parameter_table);
        }

        GaussianCPD::GaussianCPD(
            std::vector<std::string>& parent_node_label_order,
            Eigen::MatrixXd& parameter_values)
            : ContinuousCPD(parent_node_label_order) {

            this->init_from_matrix(parameter_values);
        }

        GaussianCPD::GaussianCPD(
            std::vector<std::string>&& parent_node_label_order,
            Eigen::MatrixXd&& parameter_values)
            : ContinuousCPD(std::move(parent_node_label_order)) {

            this->init_from_matrix(parameter_values);
        }

        GaussianCPD::~GaussianCPD() {}

        //----------------------------------------------------------------------
        // Copy & Move constructors/assignments
        //----------------------------------------------------------------------
        GaussianCPD::GaussianCPD(const GaussianCPD& cpd) {
            this->copy_from_cpd(cpd);
        }

        GaussianCPD& GaussianCPD::operator=(const GaussianCPD& cpd) {
            this->copy_from_cpd(cpd);
            return *this;
        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void GaussianCPD::init_from_table(
            std::vector<GaussianParameters>& parameter_table) {

            for (int i = 0; i < parameter_table.size(); i++) {
                std::vector<std::shared_ptr<Node>> parameters(2);
                parameters.push_back(std::move(parameter_table[i].mean));
                parameters.push_back(std::move(parameter_table[i].variance));
                this->parameter_table.push_back(std::move(parameters));
            }
        }

        void GaussianCPD::init_from_matrix(const Eigen::MatrixXd& matrix) {
            for (int row = 0; row < matrix.rows(); row++) {
                double mean = matrix(row, PARAMETER_INDEX::mean);
                double variance = matrix(row, PARAMETER_INDEX::variance);

                std::vector<std::shared_ptr<Node>> parameters(2);
                parameters.push_back(
                    std::make_shared<ConstantNode>(ConstantNode(mean)));
                parameters.push_back(
                    std::make_shared<ConstantNode>(ConstantNode(variance)));
                this->parameter_table.push_back(std::move(parameters));
            }
        }

        Eigen::VectorXd GaussianCPD::sample_from_table_row(
            std::shared_ptr<gsl_rng> random_generator, int table_row) const {
            Eigen::VectorXd sample_vector(1);

            double mean =
                this->parameter_table[table_row][PARAMETER_INDEX::mean]
                    ->get_assignment()(0);
            double variance =
                this->parameter_table[table_row][PARAMETER_INDEX::variance]
                    ->get_assignment()(0);
            double sample =
                mean + gsl_ran_gaussian(random_generator.get(), variance);
            sample_vector(0) = sample;

            return sample_vector;
        }

        std::unique_ptr<CPD> GaussianCPD::clone() const {
            return std::make_unique<GaussianCPD>(*this);
        }

        std::shared_ptr<CPD> GaussianCPD::clone_shared() const {
            return std::make_shared<GaussianCPD>(*this);
        }

        std::string GaussianCPD::get_description() const {
            std::stringstream ss;

            ss << "Gaussian CPD: {\n";
            for (auto& parameters : this->parameter_table) {
                ss << " Gaussian(" << *parameters[PARAMETER_INDEX::mean] << ","
                   << *parameters[PARAMETER_INDEX::variance] << ")"
                   << "\n";
            }
            ss << "}";

            return ss.str();
        }

        //----------------------------------------------------------------------
        // Remove definitions
        //----------------------------------------------------------------------

        // No definitions in this file

    } // namespace model
} // namespace tomcat