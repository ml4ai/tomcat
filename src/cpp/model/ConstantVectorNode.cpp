#include "ConstantVectorNode.h"
#include <boost/algorithm/string/join.hpp>
#include <boost/range/adaptor/transformed.hpp>
#include <iomanip>
#include <sstream>
#include <string>
#include <utility>

namespace tomcat {
    namespace model {

        std::vector<double> ConstantVectorNode::sample() const {
            return this->values;
        }

        void ConstantVectorNode::print(std::ostream &os) const {
            // Lambda function to convert the list of numbers to a list of
            // strings with 4 decimal points
            auto to_string = [](double number) -> std::string {
              std::stringstream stream;
              stream << std::fixed << std::setprecision(4) << number;
              return stream.str();
            };

            // Concatenation of a list of strings to a single string
            std::string list_of_numbers = boost::algorithm::join(
                values | boost::adaptors::transformed(to_string), ", ");

            os << "Constant([" << list_of_numbers << "])";
        }


    } // namespace model
} // namespace tomcat
