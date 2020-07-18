#include "RandomVariableNumericNode.h"
#include <utility>

namespace tomcat {
    namespace model {

        double RandomVariableNumericNode::get_assignment() const {
            return this->assignment;
        }

        void RandomVariableNumericNode::print(std::ostream& os) const {
            os << "RV(" << this->metadata->label << ", " << this->time_step
               << ")";
        }

    } // namespace model
} // namespace tomcat
