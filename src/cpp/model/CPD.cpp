#include "CPD.h"

namespace tomcat {
    namespace model {

        //----------------------------------------------------------------------
        // Definitions
        //----------------------------------------------------------------------

        // No definitions in this file

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        CPD::CPD() {}

        CPD::CPD(std::vector<std::shared_ptr<NodeMetadata>>& parent_node_order)
            : parent_node_order(parent_node_order) {
            this->init_id();
            this->fill_indexing_mapping();
        }

        CPD::CPD(std::vector<std::shared_ptr<NodeMetadata>>&& parent_node_order)
            : parent_node_order(std::move(parent_node_order)) {
            this->init_id();
            this->fill_indexing_mapping();
        }

        CPD::CPD(std::vector<std::shared_ptr<NodeMetadata>>& parent_node_order,
                 std::vector<std::shared_ptr<Distribution>>& distributions)
            : parent_node_order(parent_node_order),
              distributions(distributions) {
            this->init_id();
            this->fill_indexing_mapping();
        }

        CPD::CPD(std::vector<std::shared_ptr<NodeMetadata>>&& parent_node_order,
                 std::vector<std::shared_ptr<Distribution>>&& distributions)
            : parent_node_order(std::move(parent_node_order)),
              distributions(std::move(distributions)) {
            this->init_id();
            this->fill_indexing_mapping();
        }

        CPD::~CPD() {}

        //----------------------------------------------------------------------
        // Operator overload
        //----------------------------------------------------------------------
        std::ostream& operator<<(std::ostream& os, const CPD& cpd) {
            cpd.print(os);
            return os;
        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void CPD::init_id() {
            std::vector<std::string> labels;
            labels.reserve(this->parent_node_order.size());

            for (const auto& metadata : this->parent_node_order) {
                labels.push_back(metadata->get_label());
            }

            std::sort(labels.begin(), labels.end());
            std::stringstream ss;
            copy(labels.begin(),
                 labels.end(),
                 std::ostream_iterator<std::string>(ss, ","));
            this->id = ss.str();
        }

        void CPD::fill_indexing_mapping() {
            int cum_cardinality = 1;

            for (int order = this->parent_node_order.size() - 1; order >= 0;
                 order--) {
                std::shared_ptr<NodeMetadata> metadata =
                    this->parent_node_order[order];

                ParentIndexing indexing;
                indexing.order = order;
                indexing.right_cumulative_cardinality = cum_cardinality;

                this->parent_label_to_indexing[metadata->get_label()] =
                    indexing;

                cum_cardinality *= metadata->get_cardinality();
            }
        }

        void CPD::copy_cpd(const CPD& cpd) {
            this->id = cpd.id;
            this->updated = cpd.updated;
            this->parent_label_to_indexing = cpd.parent_label_to_indexing;
            this->parent_node_order = cpd.parent_node_order;
            this->distributions = cpd.distributions;
        }

        void CPD::update_dependencies(Node::NodeMap& parameter_nodes_map,
                                      int time_step) {

            for (auto& distribution : this->distributions) {
                distribution->update_dependencies(parameter_nodes_map,
                                                  time_step);
            }

            this->updated = true;
        }

        Eigen::MatrixXd CPD::sample(std::shared_ptr<gsl_rng> random_generator,
                                    const Node::NodeMap& parent_labels_to_nodes,
                                    int num_samples) const {

            std::vector<int> distribution_indices =
                this->get_distribution_indices(parent_labels_to_nodes,
                                               num_samples);

            int sample_size = this->distributions[0]->get_sample_size();

            Eigen::MatrixXd samples(distribution_indices.size(), sample_size);
            int i = 0;
            for (const auto& distribution_idx : distribution_indices) {
                samples.row(i++) =
                    this->distributions[distribution_idx]->sample(
                        random_generator);
            }

            return samples;
        }

        std::vector<int> CPD::get_distribution_indices(
            const Node::NodeMap& parent_labels_to_nodes,
            int num_samples) const {

            std::vector<int> indices(num_samples, 0);

            for (const auto& mapping : parent_labels_to_nodes) {
                std::string label = mapping.first;
                std::shared_ptr<Node> node = mapping.second;
                ParentIndexing indexing =
                    this->parent_label_to_indexing.at(label);

                Eigen::MatrixXd matrix = node->get_assignment();
                for (int i = 0; i < num_samples; i++) {
                    // Non-in-plate nodes will have a single assignment while
                    // in-plate nodes can have multiple assignments. The value
                    // of a non-in-plate node must be broadcasted.
                    int row = matrix.rows() == 1 ? 0 : i;

                    indices[i] += static_cast<int>(matrix(row, 0)) *
                                  indexing.right_cumulative_cardinality;
                }
            }

            return indices;
        }

        void CPD::reset_updated_status() { this->updated = false; }

        void CPD::print(std::ostream& os) const {
            os << this->get_description();
        }

        //------------------------------------------------------------------
        // Getters & Setters
        //------------------------------------------------------------------
        const std::string& CPD::get_id() const { return id; }

        bool CPD::is_updated() const { return updated; }

        const std::unordered_map<std::string, ParentIndexing>&
        CPD::get_parent_label_to_indexing() const {
            return parent_label_to_indexing;
        }

    } // namespace model
} // namespace tomcat
