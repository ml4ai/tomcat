#include "ContinuousCPD.h"

namespace tomcat {
    namespace model {

        void ContinuousCPD::copy_from_cpd(const ContinuousCPD &cpd)  {
            this->parent_node_label_order = cpd.parent_node_label_order;
            this->parameter_table = cpd.parameter_table;
        }

        void ContinuousCPD::update_dependencies() {
            // todo
        }

    } // namespace model
} // namespace tomcat
