#include "CategoricalCPD.h"

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
        CategoricalCPD::CategoricalCPD(
            std::vector<std::string>& parent_node_label_order,
            std::vector<std::shared_ptr<Node>>& cpd_table)
            : CPD(parent_node_label_order), probability_table(cpd_table) {}

        CategoricalCPD::CategoricalCPD(
            std::vector<std::string>&& parent_node_label_order,
            std::vector<std::shared_ptr<Node>>&& cpd_table)
            : CPD(std::move(parent_node_label_order)),
              probability_table(std::move(cpd_table)) {}

        CategoricalCPD::CategoricalCPD(
            std::vector<std::string>& parent_node_label_order,
            const Eigen::MatrixXd& cpd_table)
            : CPD(parent_node_label_order) {
            this->init_from_matrix(cpd_table);
        }

        CategoricalCPD::~CategoricalCPD() {}

        //----------------------------------------------------------------------
        // Copy & Move constructors/assignments
        //----------------------------------------------------------------------
        CategoricalCPD::CategoricalCPD(const CategoricalCPD& cpd) {
            this->copy_from_cpd(cpd);
        }

        CategoricalCPD& CategoricalCPD::operator=(const CategoricalCPD& cpd) {
            this->copy_from_cpd(cpd);
            return *this;
        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void CategoricalCPD::init_from_matrix(const Eigen::MatrixXd& matrix) {
            for (int row = 0; row < matrix.rows(); row++) {
                Eigen::VectorXd probabilities(matrix.cols());

                for (int col = 0; col < matrix.cols(); col++) {
                    probabilities(col) = matrix(row, col);
                }

                std::shared_ptr<Node> probabilities_node =
                    std::make_shared<ConstantNode>(std::move(probabilities));
                this->probability_table.push_back(
                    std::move(probabilities_node));
            }
        }

        void CategoricalCPD::copy_from_cpd(const CategoricalCPD& cpd) {
            this->parent_node_label_order = cpd.parent_node_label_order;
            this->probability_table = cpd.probability_table;
        }

        void CategoricalCPD::update_dependencies(
            Node::NodeMap& parameter_nodes_map, int time_step) {

            for (int i = 0; i < this->probability_table.size(); i++) {
                std::string parameter_timed_name;
                const NodeMetadata* metadata =
                    this->probability_table[i]->get_metadata().get();
                if (metadata->is_replicable()) {
                    parameter_timed_name =
                        metadata->get_timed_name(time_step);
                }
                else {
                    parameter_timed_name =
                        metadata->get_timed_name(
                            metadata->get_initial_time_step());
                }

                if (parameter_nodes_map.count(parameter_timed_name) > 0) {
                    this->probability_table[i] =
                        parameter_nodes_map[parameter_timed_name];
                }
            }
            this->updated = true;
        }

        std::unique_ptr<CPD> CategoricalCPD::clone() const {
            return std::make_unique<CategoricalCPD>(*this);
        }

        std::shared_ptr<CPD> CategoricalCPD::clone_shared() const {
            return std::make_shared<CategoricalCPD>(*this);
        }

        std::string CategoricalCPD::get_description() const {
            std::stringstream ss;
            ss << "Categorical CPD: {\n";
            for (auto& probabilities : this->probability_table) {
                ss << " " << *probabilities << "\n";
            }
            ss << "}";

            return ss.str();
        }

        Eigen::VectorXd CategoricalCPD::sample_from_table_row(
            std::shared_ptr<gsl_rng> random_generator, int table_row) const {
            int k = this->probability_table[table_row]
                        ->get_metadata()
                        ->get_sample_size();
            const double* probabilities =
                this->probability_table[table_row]->get_assignment().data();

            unsigned int* sample_ptr = new unsigned int[k];
            gsl_ran_multinomial(
                random_generator.get(), k, 1, probabilities, sample_ptr);

            Eigen::VectorXd sample_vector(1);
            sample_vector(0) = this->get_sample_index(sample_ptr, k);

            delete[] sample_ptr;

            return sample_vector;
        }

        unsigned int
        CategoricalCPD::get_sample_index(const unsigned int* sample_array,
                                         size_t array_size) const {
            return std::distance(
                sample_array,
                std::find(sample_array, sample_array + array_size, 1));
        }

    } // namespace model
} // namespace tomcat