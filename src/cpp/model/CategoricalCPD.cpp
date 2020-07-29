#include "CategoricalCPD.h"
#include "ConstantNode.h"
#include <string>

namespace tomcat {
    namespace model {

        void CategoricalCPD::copy_from_cpd(const CategoricalCPD& cpd) {
            this->parent_node_label_order = cpd.parent_node_label_order;

            // Clone each node in the copied cpd to the probability table of
            // this object.
            this->probability_table.reserve(cpd.probability_table.size());
            for (const auto& source_probabilities : cpd.probability_table) {
                std::unique_ptr<Node> node = source_probabilities->clone();
                this->probability_table.push_back(std::move(node));
            }
        }

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

        Eigen::MatrixXd
        CategoricalCPD::sample(std::shared_ptr<gsl_rng> generator) const {
            Eigen::VectorXd samples(this->probability_table.size());

            for (int i = 0; i < this->probability_table.size(); i++) {
                const double* probabilities =
                    this->probability_table[i]->get_assignment().data();

                unsigned int* sample_ptr = new unsigned int[1];
                gsl_ran_multinomial(generator.get(),
                                    this->probability_table.size(),
                                    1,
                                    probabilities,
                                    sample_ptr);
                samples(i) = sample_ptr[0];
                delete[] sample_ptr;
            }

            return samples;
        }

        void CategoricalCPD::print(std::ostream& os) const {
            os << "Categorical CPD: {\n";
            for (auto& probabilities : this->probability_table) {
                os << " " << *probabilities << "\n";
            }
            os << "}";
        }

        std::unique_ptr<CPD> CategoricalCPD::clone() const {
            return std::make_unique<CategoricalCPD>(*this);
        }

        void CategoricalCPD::update_dependencies(NodeMap& parameter_nodes_map,
                                                 int time_step) {

            for (int i = 0; i < this->probability_table.size(); i++) {
                std::string parameter_timed_name;
                NodeMetadata* metadata =
                    this->probability_table[i]->get_metadata().get();
                if (metadata->is_replicable()) {
                    parameter_timed_name =
                        this->probability_table[i]->get_timed_name(time_step);
                }
                else {
                    parameter_timed_name =
                        this->probability_table[i]->get_timed_name(
                            metadata->get_initial_time_step());
                }

                if (parameter_nodes_map.count(parameter_timed_name) > 0) {
                    this->probability_table[i] =
                        parameter_nodes_map[parameter_timed_name];
                }
            }
            this->updated = true;
        }

    } // namespace model
} // namespace tomcat