#pragma once

#include <eigen3/Eigen/Dense>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_rng.h>
#include <vector>

namespace tomcat {
    namespace model {

        /**
         * Abstract representation of a conditional probability distribution.
         *
         * In a CPD table, each row consists of the distribution of a child node
         * given its parents assignments. For the discrete case, each row
         * consists of probabilities that define a categorical distribution. For
         * the continuous case, each row contains the parameters of the
         * underlying distribution.
         *
         */
        class CPD {
          protected:
            // It defines the order of the parent nodes in the cartesian
            // product of their possible assignments. It's necessary to know
            // this order for correctly index a distribution given a parent
            // assignment.
            std::vector<std::string> parent_node_label_order;

          public:
            /**
             * Abstract representation of a Conditional Probability Distribution
             *
             * @param parent_node_label_order: evaluation order of the parent
             * nodes assignment for correct table indexing
             */
            CPD(std::vector<std::string> parent_node_label_order)
                : parent_node_label_order(std::move(parent_node_label_order)) {}

            virtual ~CPD() {}

            /**
             * Draw a sample from each distribution of the CPD table.
             *
             * @param generator: random number generator
             * @return matrix or vector with the samples
             */
            virtual Eigen::MatrixXd
            sample(std::shared_ptr<gsl_rng> generator) const = 0;

            /**
             * Print a short description of the distribution.
             *
             * @param os: output stream
             */
            virtual void print(std::ostream& os) const {}

            friend std::ostream& operator<<(std::ostream& os, const CPD& cpd) {
                cpd.print(os);
                return os;
            };
        };

    } // namespace model
} // namespace tomcat
