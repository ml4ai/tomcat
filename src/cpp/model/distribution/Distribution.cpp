#include "model/distribution/Distribution.h"

namespace tomcat {
    namespace model {

        using namespace std;

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        Distribution::Distribution() {}

        Distribution::~Distribution() {}

        //----------------------------------------------------------------------
        // Operator overload
        //----------------------------------------------------------------------
        ostream& operator<<(ostream& os,
                                 const Distribution& distribution) {
            distribution.print(os);
            return os;
        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void Distribution::print(ostream& os) const {
            os << this->get_description();
        }

    } // namespace model
} // namespace tomcat
