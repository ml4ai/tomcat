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

        void CPD::copy_from_cpd(const CPD& cpd) {
            this->id = cpd.id;
            this->updated = cpd.updated;
            this->parent_label_to_indexing = cpd.parent_label_to_indexing;
        }

        Eigen::VectorXd
        CPD::sample(std::shared_ptr<gsl_rng> random_generator,
                    const Node::NodeMap& parent_labels_to_nodes) const {

            int table_row = this->get_table_row_given_parents_assignments(
                parent_labels_to_nodes);

            return this->sample_from_table_row(random_generator, table_row);
        }

        //        Eigen::VectorXd CPD::sample_weighted(
        //            std::shared_ptr<gsl_rng> random_generator,
        //            const Node::NodeMap& parent_labels_to_nodes,
        //            const std::vector<std::shared_ptr<RandomVariableNode>>&
        //            child_nodes) const {
        //
        //            int table_row =
        //            this->get_table_row_given_parents_assignments(
        //                parent_labels_to_nodes);
        //
        //            // TODO - adjust for weighted cases
        //            return this->sample_from_table_row(random_generator,
        //            table_row);
        //        }

        int CPD::get_table_row_given_parents_assignments(
            const Node::NodeMap& parent_labels_to_nodes) const {

            int distribution_index = 0;

            for (const auto& mapping : parent_labels_to_nodes) {
                std::string label = mapping.first;
                std::shared_ptr<Node> node = mapping.second;
                ParentIndexing indexing =
                    this->parent_label_to_indexing.at(label);

                distribution_index +=
                    static_cast<int>(node->get_assignment()[0]) *
                    indexing.right_cumulative_cardinality;
            }

            return distribution_index;
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
