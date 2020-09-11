#pragma once

#include "model/utils/Definitions.h"
#include "model/distribution/Continuous.h"

namespace tomcat {
    namespace model {

        /**
         * Gaussian distribution with mean and variance defined by two nodes,
         * which can be constant or random variables.
         */
        class Gaussian : public Continuous {
          public:
            //------------------------------------------------------------------
            // Types, Enums & Constants
            //------------------------------------------------------------------
            enum PARAMETER_INDEX { mean, variance };

            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates an instance of a Gaussian distribution for node
             * dependent parameters.
             *
             * @param parameters: nodes which the assignments define the mean
             * and the variance of the distribution
             */
            Gaussian(std::shared_ptr<Node>& mean,
                     std::shared_ptr<Node>& variance);

            /**
             * Creates an instance of a Gaussian distribution for node
             * dependent parameters.
             *
             * @param parameters: nodes which the assignments define the mean
             * and the variance of the distribution
             */
            Gaussian(std::shared_ptr<Node>&& mean,
                     std::shared_ptr<Node>&& variance);

            /**
             * Creates an instance of a Gaussian distribution by transforming
             * a constant 2D-vector into a constant node for the mean and
             * another to the variance in order to keep static and node
             * dependent distributions compatible.
             *
             * @param parameters: Mean and variance of a Gaussian distribution
             */
            Gaussian(const Eigen::VectorXd& parameters);

            ~Gaussian();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------
            Gaussian(const Gaussian& gaussian);

            Gaussian& operator=(const Gaussian& gaussian);

            Gaussian(Gaussian&&) = default;

            Gaussian& operator=(Gaussian&&) = default;

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------
            Eigen::VectorXd sample(std::shared_ptr<gsl_rng> random_generator,
                                   int parameter_idx) const override;

            Eigen::VectorXd
            sample(std::shared_ptr<gsl_rng> random_generator,
                   int parameter_idx,
                   const Eigen::VectorXd& weights) const override;

            Eigen::VectorXd sample_from_conjugacy(
                std::shared_ptr<gsl_rng> random_generator,
                int parameter_idx,
                const Eigen::VectorXd& sufficient_statistics) const override;

            double get_pdf(const Eigen::VectorXd& value,
                           int parameter_idx) const override;

            std::unique_ptr<Distribution> clone() const override;

            std::string get_description() const override;

            int get_sample_size() const override;

          private:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Returns the mean and the variance from assignments of the nodes
             * in the list of parameters.
             *
             * @param parameter_idx: the index of the parameter assignment
             * to use in case the distribution depend on parameter nodes with
             * multiple assignments. If the parameter has single assignment,
             * that is the one being used regardless of the value informed in
             * this argument.
             * @return Vector of containing the mean and the variance of the
             * distribution.
             */
            Eigen::VectorXd get_parameters(int parameter_idx) const;

            /**
             * Generate a sample using the GSL library.
             *
             * @param random_generator: random number generator
             * @param mean: mean or weighted mean
             * @param variance: variance or weighted variance
             * @return A sample from a Gaussian distribution.
             */
            Eigen::VectorXd
            sample_from_gsl(std::shared_ptr<gsl_rng> random_generator,
                            double mean,
                            double variance) const;
        };

    } // namespace model
} // namespace tomcat
