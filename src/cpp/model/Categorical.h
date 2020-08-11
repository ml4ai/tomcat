#pragma once

#import "Distribution.h"
#import "Node.h"

namespace tomcat {
    namespace model {

        /**
         * Class description here
         */
        class Categorical : public Distribution {
          public:
            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates an instance of a categorical distribution for node
             * dependent probabilities.
             *
             * @param probabilities: node which the assignment defines the set
             * of probabilities of the distribution
             */
            Categorical(std::shared_ptr<Node>& probabilities);

            /**
             * Creates an instance of a categorical distribution for node
             * dependent probabilities.
             *
             * @param probabilities: node which the assignment defines the set
             * of probabilities of the distribution
             */
            Categorical(std::shared_ptr<Node>&& probabilities);

            /**
             * Creates an instance of a categorical distribution by transforming a
             * numerical vector of probabilities into a constant node to keep
             * static and node dependent distributions compatible.
             *
             * @param probabilities: Vector of constant probabilities
             */
            Categorical(const Eigen::VectorXd& probabilities);

            /**
             * Creates an instance of a categorical distribution by transforming a
             * numerical vector of probabilities into a constant node to keep
             * static and node dependent distributions compatible.
             *
             * @param probabilities: Vector of constant probabilities
             */
            Categorical(const Eigen::VectorXd&& probabilities);

            ~Categorical();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------
            Categorical(const Categorical& categorical);

            Categorical& operator=(const Categorical& categorical);

            Categorical(Categorical&&) = default;

            Categorical& operator=(Categorical&&) = default;

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------
            Eigen::VectorXd
            sample(std::shared_ptr<gsl_rng> random_generator) const override;

            Eigen::VectorXd sample(std::shared_ptr<gsl_rng> random_generator,
                                   Eigen::VectorXd log_weights) const override;

            double get_pdf(Eigen::VectorXd value) const override;

            std::unique_ptr<Distribution> clone() const override;

            std::string get_description() const override;

            //------------------------------------------------------------------
            // Getters & Setters
            //------------------------------------------------------------------
            const std::shared_ptr<Node>& get_probabilities() const;

            void set_probabilities(const std::shared_ptr<Node>& probabilities);

          private:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Generate a sample using the GSL library.
             *
             * @param random_generator: random number generator
             * @param parameters: probabilities or weighted probabilities
             * @return A sample from a categorical distribution.
             */
            Eigen::VectorXd
            sample_from_gsl(std::shared_ptr<gsl_rng> random_generator,
                            const Eigen::VectorXd& parameters) const;

            /**
             * Returns the index of a sampled value from a one-hot-encode array.
             *
             * @param sample_array: one-hot-encode sample
             * @param array_size: size of the one-hot-encode sample
             * @return Index containing 1 in an one-hot-encode array
             */
            unsigned int get_sample_index(const unsigned int* sample_array,
                                          size_t array_size) const;

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------

            // The assignment of a node defines the probabilities
            std::shared_ptr<Node> probabilities;
        };

    } // namespace model
} // namespace tomcat
