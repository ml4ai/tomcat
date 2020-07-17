#include "RandomVariableNumericNode.h"
#include <utility>

namespace tomcat {
    namespace model {

        // todo - Sample from a distribution
        Eigen::MatrixXd RandomVariableNumericNode::sample() const {
            return Eigen::MatrixXd();
        }

        void RandomVariableNumericNode::print(std::ostream& os) const {
            os << "RV(" << this->metadata->label << ", " << this->time_step
               << ")";
        }

    } // namespace model
} // namespace tomcat
