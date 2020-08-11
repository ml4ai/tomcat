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
            std::vector<std::shared_ptr<NodeMetadata>>& parent_node_order,
            std::vector<std::shared_ptr<Categorical>>& cpd_table)
            : CPD(parent_node_order), probability_table(cpd_table) {}

        CategoricalCPD::CategoricalCPD(
            std::vector<std::shared_ptr<NodeMetadata>>&& parent_node_order,
            std::vector<std::shared_ptr<Categorical>>&& cpd_table)
            : CPD(std::move(parent_node_order)),
              probability_table(std::move(cpd_table)) {}

        CategoricalCPD::CategoricalCPD(
            std::vector<std::shared_ptr<NodeMetadata>>& parent_node_order,
            const Eigen::MatrixXd& cpd_table)
            : CPD(parent_node_order) {
            this->init_from_matrix(cpd_table);
        }

        CategoricalCPD::CategoricalCPD(
            std::vector<std::shared_ptr<NodeMetadata>>&& parent_node_order,
            const Eigen::MatrixXd&& cpd_table)
            : CPD(parent_node_order) {
            this->init_from_matrix(std::move(cpd_table));
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
            for (int i = 0; i < matrix.rows(); i++) {
                std::shared_ptr<Categorical> distribution_ptr =
                    std::make_shared<Categorical>(
                        Categorical(std::move(matrix.row(i))));
                this->probability_table.push_back(distribution_ptr);
            }
        }

        void CategoricalCPD::copy_from_cpd(const CategoricalCPD& cpd) {
            CPD::copy_from_cpd(cpd);
            this->parent_node_order = cpd.parent_node_order;
            this->probability_table = cpd.probability_table;
        }

        void
        CategoricalCPD::update_dependencies(Node::NodeMap& parameter_nodes_map,
                                            int time_step) {

            for (int i = 0; i < this->probability_table.size(); i++) {

                std::string parameter_timed_name;
                const NodeMetadata* metadata = this->probability_table[i]
                                                   ->get_probabilities()
                                                   ->get_metadata()
                                                   .get();
                if (metadata->is_replicable()) {
                    parameter_timed_name = metadata->get_timed_name(time_step);
                }
                else {
                    parameter_timed_name = metadata->get_timed_name(
                        metadata->get_initial_time_step());
                }

                if (parameter_nodes_map.count(parameter_timed_name) > 0) {
                    this->probability_table[i]->set_probabilities(
                        parameter_nodes_map[parameter_timed_name]);
                }
            }
            this->updated = true;
        }

        std::unique_ptr<CPD> CategoricalCPD::clone() const {
            std::unique_ptr<CategoricalCPD> new_cpd =
                std::make_unique<CategoricalCPD>(*this);
            new_cpd->clone_distributions();
            return new_cpd;
        }

        void CategoricalCPD::clone_distributions() {
            for (auto& distribution : this->probability_table) {
                std::shared_ptr<Distribution> temp = distribution->clone();
                distribution = std::dynamic_pointer_cast<Categorical>(temp);
            }
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

            return this->probability_table[table_row]->sample(random_generator);
        }

    } // namespace model
} // namespace tomcat