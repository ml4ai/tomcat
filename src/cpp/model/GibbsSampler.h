#pragma once

#include "Sampler.h"

namespace tomcat {
    namespace model {

        //------------------------------------------------------------------
        // Forward declarations
        //------------------------------------------------------------------

        //------------------------------------------------------------------
        // Structs
        //------------------------------------------------------------------

        /**
         * Class description here
         */
        class GibbsSampler : public Sampler {
          public: // same pattern for protected and private in this order
            //------------------------------------------------------------------
            // Types, Enums & Constants
            //------------------------------------------------------------------

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
            GibbsSampler(DynamicBayesNet model,
                         std::shared_ptr<gsl_rng> random_generator,
                         int burn_in_period);

            ~GibbsSampler();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------
            GibbsSampler(const GibbsSampler& sampler);

            GibbsSampler& operator=(const GibbsSampler& sampler);

            GibbsSampler(GibbsSampler&&) = default;

            GibbsSampler& operator=(GibbsSampler&&) = default;

            //------------------------------------------------------------------
            // Operator overload
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Static functions
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------
            void sample_latent(int num_samples) override;

            //------------------------------------------------------------------
            // Virtual functions
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Pure virtual functions
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Getters & Setters
            //------------------------------------------------------------------

          private:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------
            void fill_initial_samples();

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------
            int burn_in_period = 0;
        };

    } // namespace model
} // namespace tomcat
