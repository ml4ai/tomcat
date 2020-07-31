#include "CategoricalCPD.h"
#include "ConstantNode.h"
#include <string>

namespace tomcat {
    namespace model {

        void CategoricalCPD::copy_from_cpd(const CategoricalCPD& cpd) {
            this->parent_node_label_order = cpd.parent_node_label_order;
            this->probability_table = cpd.probability_table;
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

        Eigen::MatrixXd CategoricalCPD::sample(
            std::shared_ptr<gsl_rng> random_generator) const {
            Eigen::VectorXd samples(this->probability_table.size());

            for (int i = 0; i < this->probability_table.size(); i++) {
                samples(i) = this->sample(random_generator, i)(0);
            }

            return samples;
        }

        static unsigned int get_sample_index(const unsigned int* sample_array,
                                             size_t array_size) {
            return std::distance(
                sample_array,
                std::find(sample_array, sample_array + array_size, 1));
        }

        Eigen::VectorXd
        CategoricalCPD::sample(std::shared_ptr<gsl_rng> random_generator,
                               int index) const {
            int k = this->probability_table[index]
                        ->get_metadata()
                        ->get_sample_size();
            const double* probabilities =
                this->probability_table[index]->get_assignment().data();

            unsigned int* sample_ptr = new unsigned int[k];
            gsl_ran_multinomial(
                random_generator.get(), k, 1, probabilities, sample_ptr);

            Eigen::VectorXd sample_vector(1);
            sample_vector(0) = get_sample_index(sample_ptr, k);

            delete[] sample_ptr;

            return sample_vector;
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

        std::shared_ptr<CPD> CategoricalCPD::clone_shared() const {
            return std::make_shared<CategoricalCPD>(*this);
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