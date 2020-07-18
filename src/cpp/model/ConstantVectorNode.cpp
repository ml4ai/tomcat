#include "ConstantVectorNode.h"
#include <boost/algorithm/string/join.hpp>
#include <boost/range/adaptor/transformed.hpp>
#include <iomanip>
#include <sstream>
#include <string>
#include <utility>

namespace tomcat {
    namespace model {

        Eigen::VectorXd ConstantVectorNode::get_assignment() const {
            return this->values;
        }

        void ConstantVectorNode::print(std::ostream& os) const {
            os << "Constant([" << this->values.transpose() << "])";
        }

    } // namespace model
} // namespace tomcat
