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

        CPD::CPD(std::vector<std::string> parent_node_label_order)
        : parent_node_label_order(std::move(parent_node_label_order)) {}

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
        Eigen::VectorXd
        CPD::sample(std::shared_ptr<gsl_rng> random_generator,
               const Node::NodeMap& parent_labels_to_nodes) const {

            int table_row = this->get_table_row_given_parents_assignments(
                parent_labels_to_nodes);

            return this->sample_from_table_row(random_generator, table_row);
        }

        int CPD::get_table_row_given_parents_assignments(
            const Node::NodeMap& parent_labels_to_nodes) const {
            int least_significant_cum_cardinality = 1;
            int distribution_index = 0;

            // Iterate in reverse order
            for (auto parent_label_ptr =
                this->parent_node_label_order.rbegin();
                 parent_label_ptr != this->parent_node_label_order.rend();
                 parent_label_ptr++) {

                // TODO - it does not work with multidimensional sample size
                std::shared_ptr<Node> parent_node =
                    parent_labels_to_nodes.at(*parent_label_ptr);
                int assignment =
                    static_cast<int>(parent_node->get_assignment()[0]);
                distribution_index +=
                    assignment * least_significant_cum_cardinality;
                least_significant_cum_cardinality *=
                    parent_node->get_metadata()->get_cardinality();
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
        bool CPD::is_updated() const { return this->updated; }

        const std::vector<std::string>&
        CPD::get_parent_node_label_order() const {
            return parent_node_label_order;
        }

    } // namespace model
} // namespace tomcat
