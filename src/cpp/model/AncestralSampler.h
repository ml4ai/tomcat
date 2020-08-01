#pragma once

#include "Sampler.h"

namespace tomcat {
    namespace model {

        class Sampler;

        /**
         * Generates samples by ancestral sampling process. When data is
         * provided for specific nodes, samples are not generated for these
         * nodes and the samples generated for the latent nodes will be
         * conditioned on their parent node's data if provided.
         */
        class AncestralSampler : public Sampler {
          public:
            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates an instance of the sampler for a given model and random
             * number generator.
             *
             * @param model: DBN
             * @param random_generator: Random number generator
             */
            AncestralSampler(DynamicBayesNet model,
                             std::shared_ptr<gsl_rng> random_generator);

            ~AncestralSampler();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------
            AncestralSampler(const AncestralSampler& sampler);

            AncestralSampler& operator=(const AncestralSampler& sampler);

            AncestralSampler(AncestralSampler&&) = default;

            AncestralSampler& operator=(AncestralSampler&&) = default;

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------
            void sample(int num_samples, int time_steps) override;

            std::unique_ptr<Sampler> clone() override;

          private:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Samples a value from a given node's CPD given its parents
             * assignments.
             *
             * @param node: random variable node to sample from
             * @return Sample from the node's distribution given its parents'
             * assignments.
             */
            Eigen::VectorXd
            get_sample_given_parents_for(const RandomVariableNode& node) const;

            /**
             * Assigns a value from a given data point (row in the data matrix)
             * to its corresponding node in the unrolled DBN. Each node in the
             * unrolled DBN has a time step assigned to it, which determines the
             * column (in the data matrix) from which the value has to be picked
             * from. Each node can only assume one value at a time so this
             * method has to be called separately for each one of the data
             * points in the data matrix.
             *
             * @param data_point_index: index of the data point in the data
             * matrix
             */
            void
            assign_data_to_node(const std::shared_ptr<RandomVariableNode>& node,
                                int data_point_index);
        };
    } // namespace model
} // namespace tomcat