#include "Estimates.h"

namespace tomcat {
    namespace model {

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        Estimates::Estimates(std::shared_ptr<Estimator> estimator) : Measure(estimator) {}

        Estimates::~Estimates() {}

        //----------------------------------------------------------------------
        // Copy & Move constructors/assignments
        //----------------------------------------------------------------------
        Estimates::Estimates(
            const Estimates& estimates) {
            this->copy_measure(estimates);
        }

        Estimates& Estimates::operator=(const Estimates& estimates) {
            this->copy_measure(estimates);
            return *this;
        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        std::string Estimates::get_name() const {
            return "Estimates";
        }

        DBNData Estimates::evaluate(const DBNData& test_data) const {
            return this->estimator->get_last_estimates(0);
        }

    } // namespace model
} // namespace tomcat
