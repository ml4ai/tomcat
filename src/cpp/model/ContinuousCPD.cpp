#include "ContinuousCPD.h"

namespace tomcat {
    namespace model {

        void ContinuousCPD::copy_from_cpd(const ContinuousCPD& cpd) {
            this->parent_node_label_order = cpd.parent_node_label_order;
            this->parameter_table = cpd.parameter_table;
        }

        void ContinuousCPD::update_dependencies(NodeMap& parameter_nodes_map,
                                                int time_step) {
            for (int i = 0; i < this->parameter_table.size(); i++) {
                for (int j = 0; j < this->parameter_table[i].size(); j++) {
                    std::string parameter_timed_name;
                    NodeMetadata* metadata =
                        this->parameter_table[i][j]->get_metadata().get();
                    if (metadata->repeatable) {
                        parameter_timed_name =
                            this->parameter_table[i][j]->get_timed_name(
                                time_step);
                    }
                    else {
                        parameter_timed_name =
                            this->parameter_table[i][j]->get_timed_name(
                                metadata->initial_time_step);
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

    } // namespace model
} // namespace tomcat
