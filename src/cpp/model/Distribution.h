#pragma once

#include "eigen3/Eigen/Dense"
#include <gsl/gsl_rng.h>

namespace tomcat {
    namespace model {

#define EPSILON 10E-16

        /**
         * Abstract probability distribution.
         */
        class Distribution {
          public:
            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates an abstract representation of a distribution.
             */
            Distribution();

            virtual ~Distribution();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------

            // Copy constructor and assignment should be deleted to avoid
            // implicit slicing and loss of polymorphic behaviour in the
            // subclasses. To deep copy, the clone method must be used.
            Distribution(const Distribution&) = delete;

            Distribution& operator=(const Distribution&) = delete;

            Distribution(Distribution&&) = default;

            Distribution& operator=(Distribution&&) = default;

            //------------------------------------------------------------------
            // Operator overload
            //------------------------------------------------------------------
            friend std::ostream& operator<<(std::ostream& os,
                                            const Distribution& distribution);

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Prints a short description of the distribution.
             *
             * @param os: output stream
             */
            void print(std::ostream& os) const;

            //------------------------------------------------------------------
            // Pure virtual functions
            //------------------------------------------------------------------

            /**
             * Draws a sample from the distribution.
             *
             * @param random_generator: random number random_generator
             *
             * @return A sample from the distribution.
             */
            virtual Eigen::VectorXd
            sample(std::shared_ptr<gsl_rng> random_generator) const = 0;

            /**
             * Draws a sample from the distribution scaled by a vector of log
             * weights.
             *
             * @param random_generator: random number random_generator
             * @param log_weights: log-values to scale the parameters of the
             * distribution
             *
             * @return A sample from the distribution.
             */
            virtual Eigen::VectorXd sample(std::shared_ptr<gsl_rng> random_generator,
                                           Eigen::VectorXd log_weights) const = 0;

            /**
             * Returns the PDF for a given value.
             *
             * @param value: a possible sample from the distribution
             * @return PDF for the value.
             */
            virtual double get_pdf(Eigen::VectorXd value) const = 0;

            /**
             * Creates a new unique pointer from a concrete instance of a distribution.
             *
             * @return Pointer to the new distribution.
             */
            virtual std::unique_ptr<Distribution> clone() const = 0;

          protected:
            //------------------------------------------------------------------
            // Pure virtual functions
            //------------------------------------------------------------------

            /**
             * Returns a short description of the distribution.
             *
             * @return Distribution's description.
             */
            virtual std::string get_description() const = 0;
        };

    } // namespace model
} // namespace tomcat
