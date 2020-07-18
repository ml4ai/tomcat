#pragma once

#include <vector>
#include <boost/random.hpp>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>

namespace tomcat {
    namespace model {

        /*
         * Abstract representation of a conditional probability distribution. T
         * defines the type of data returned by a sample from that distribution.
         * It can either be a number of a numerical vector.
         */
        template <typename T> class CPD {
          protected:
            // It defines the order of the parent nodes in the cartesian
            // product of their possible assignments. It's necessary to know
            // this order for correctly index a distribution given a parent
            // assignment.
            std::vector<std::string> parent_node_label_order;


          public:
            CPD(std::vector<std::string> parent_node_label_order)
                : parent_node_label_order(std::move(parent_node_label_order)) {}

            virtual ~CPD() {}

            virtual T sample(std::shared_ptr<gsl_rng> generator) const = 0;

            virtual void print(std::ostream& os) const {}

            friend std::ostream& operator<<(std::ostream& os,
                                            const CPD<T>& cpd) {
                cpd.print(os);
                return os;
            };
        };

    } // namespace model
} // namespace tomcat
