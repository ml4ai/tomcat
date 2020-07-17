#include "RandomVariableNumericNode.h"
#include <utility>

namespace tomcat {
    namespace model {

        // todo - Sample from a distribution
        double RandomVariableNumericNode::sample() const { return this->cpd->sample(); }

        void RandomVariableNumericNode::print(std::ostream& os) const {
            os << "RV(" << this->metadata->label << ", " << this->time_step << ")";
        }

    } // namespace model
} // namespace tomcat
