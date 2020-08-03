#include "ContinuousCPD.h"

namespace tomcat {
    namespace model {

        //----------------------------------------------------------------------
        // Definitions
        //----------------------------------------------------------------------

        // No definitions in this file

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        ContinuousCPD::ContinuousCPD() {}

        ContinuousCPD::ContinuousCPD(
            std::vector<std::string>& parent_node_label_order)
        : CPD(parent_node_label_order) {}

        ContinuousCPD::ContinuousCPD(
            std::vector<std::string>&& parent_node_label_order)
        : CPD(std::move(parent_node_label_order)) {}

        ContinuousCPD::~ContinuousCPD() {}

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void ContinuousCPD::update_dependencies(Node::NodeMap& parameter_nodes_map,
                                                int time_step) {
            for (int i = 0; i < this->parameter_table.size(); i++) {
                for (int j = 0; j < this->parameter_table[i].size(); j++) {
                    std::string parameter_timed_name;
                    NodeMetadata* metadata =
                        this->parameter_table[i][j]->get_metadata().get();
                    if (metadata->is_replicable()) {
                        parameter_timed_name =
                            metadata->get_timed_name(
                                time_step);
                    }
                    else {
                        parameter_timed_name =
                            metadata->get_timed_name(
                                metadata->get_initial_time_step());
                    }

                    if (parameter_nodes_map.count(parameter_timed_name) > 0) {
                        this->parameter_table[i][j] =
                            parameter_nodes_map[parameter_timed_name];
                    }
                    delete metadata;
                }
            }
            this->updated = true;
        }

        void ContinuousCPD::copy_from_cpd(const ContinuousCPD& cpd) {
            this->parent_node_label_order = cpd.parent_node_label_order;
            this->parameter_table = cpd.parameter_table;
        }

    } // namespace model
} // namespace tomcat
