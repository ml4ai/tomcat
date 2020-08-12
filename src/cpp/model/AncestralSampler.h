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
            void sample_latent(int num_samples) override;

        };
    } // namespace model
} // namespace tomcat