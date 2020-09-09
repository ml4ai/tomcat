#pragma once

#include <eigen3/Eigen/Dense>
#include <gsl/gsl_rng.h>

#include "model/pgm/Node.h"
#include "model/utils/Definitions.h"

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
             * Replaces parameter nodes in the distribution by the correct
             * copy of the node in an unrolled DBN.
             *
             * @param parameter_nodes_map: mapping between a parameter node's
             * timed name and its concrete object reference in an unrolled DBN
             * @param time_step: time step of the node that owns the CPD in the
             * unrolled DBN. It can be different from the time step of the
             * parameter node if the latter is shared among nodes over several
             * time steps.
             */
            virtual void update_dependencies(Node::NodeMap& parameter_nodes_map,
                                             int time_step) = 0;

            /**
             * Draws a sample from the distribution.
             *
             * @param random_generator: random number random_generator
             * @param parameter_idx: the index of the parameter assignment
             * to use in case the distribution depend on parameter nodes with
             * multiple assignments. If the parameter has single assignment,
             * that is the one being used regardless of the value informed in
             * this argument.
             *
             * @return A sample from the distribution.
             */
            virtual Eigen::VectorXd
            sample(std::shared_ptr<gsl_rng> random_generator,
                   int parameter_idx = 0) const = 0;

            /**
             * Draws a sample from the distribution scaled by a vector of
             * weights.
             *
             * @param random_generator: random number random_generator
             * @param parameter_idx: the index of the parameter assignment
             * to use in case the distribution depend on parameter nodes with
             * multiple assignments. If the parameter has single assignment,
             * that is the one being used regardless of the value informed in
             * this argument.
             * @param weights: values to scale the parameters of the
             * distribution
             *
             * @return A sample from the distribution.
             */
            virtual Eigen::VectorXd
            sample(std::shared_ptr<gsl_rng> random_generator,
                   int parameter_idx,
                   const Eigen::VectorXd& weights) const = 0;

            /**
             * Draws a sample from a posterior computed by conjugacy using
             * sufficient statistics.
             *
             * @param random_generator: random number random_generator
             * @param parameter_idx: the index of the parameter assignment
             * to use in case the distribution depend on parameter nodes with
             * multiple assignments. If the parameter has single assignment,
             * that is the one being used regardless of the value informed in
             * this argument.
             * @param sufficient_statistics: sufficient statistics needed to
             * come up with a posterior for the distribution
             *
             * @return A sample from the posterior distribution
             */
            virtual Eigen::VectorXd sample_from_conjugacy(
                std::shared_ptr<gsl_rng> random_generator,
                int parameter_idx,
                const Eigen::VectorXd& sufficient_statistics) const = 0;

            /**
             * Returns the PDF/PMFs for a given value.
             *
             * @param value: possible sample from the distribution
             * @param parameter_idx: the index of the parameter assignment
             * to use in case the distribution depend on parameter nodes with
             * multiple assignments. If the parameter has single assignment,
             * that is the one being used regardless of the value informed in
             * this argument.
             * @return PDFs for the values.
             */
            virtual double get_pdf(const Eigen::VectorXd& value,
                                   int parameter_idx) const = 0;

            /**
             * Creates a new unique pointer from a concrete instance of a
             * distribution.
             *
             * @return Pointer to the new distribution.
             */
            virtual std::unique_ptr<Distribution> clone() const = 0;

            /**
             * Returns the size of a sample generated by the distribution.
             *
             * @return Sample size.
             */
            virtual int get_sample_size() const = 0;

            /**
             * Update the sufficient statistics in the parameter nodes given the
             * assignment informed.
             *
             * @param assignment: Assignment from the data node that depends on
             * the parameter being updated
             */
            virtual void
            update_sufficient_statistics(const Eigen::VectorXd& sample) = 0;

            /**
             * Returns assignments of the node(s) the distribution depends on. A
             * node can have multiple assignments. Only the first one is
             * returned by this function.
             *
             * @return Concrete node assignments.
             */
            virtual Eigen::VectorXd get_values() const = 0;

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
