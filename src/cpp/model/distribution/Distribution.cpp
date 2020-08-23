#include "Distribution.h"

namespace tomcat {
    namespace model {

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        Distribution::Distribution() {}

        Distribution::~Distribution() {}

        //----------------------------------------------------------------------
        // Operator overload
        //----------------------------------------------------------------------
        std::ostream& operator<<(std::ostream& os,
                                 const Distribution& distribution) {
            distribution.print(os);
            return os;
        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void Distribution::print(std::ostream& os) const {
            os << this->get_description();
        }

    } // namespace model
} // namespace tomcat
