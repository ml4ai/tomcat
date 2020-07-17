#include "ConstantNumericNode.h"

namespace tomcat {
    namespace model {

        double ConstantNumericNode::sample() const { return this->value; }

        void ConstantNumericNode::print(std::ostream &os) const {
            os << "Constant(" << value << ")";
        }

    } // namespace model
} // namespace tomcat