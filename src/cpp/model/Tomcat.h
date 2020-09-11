#pragma once

#include <memory>
#include <string>

#include <gsl/gsl_rng.h>

#include "model/pgm/DynamicBayesNet.h"
#include "model/utils/Definitions.h"

namespace tomcat {
    namespace model {

        /**
         * ToMCAT PGM
         */
        class Tomcat {
          public:
            //------------------------------------------------------------------
            // Types, Enums & Constants
            //------------------------------------------------------------------

            // Node labels
            inline static const std::string THETA_S = "Theta_S";
            inline static const std::string PI_RM = "Pi_RM";
            inline static const std::string PI_SG = "Pi_SG";
            inline static const std::string PI_SY = "Pi_SY";
            inline static const std::string STATE = "State";
            inline static const std::string ROOM = "Room";
            inline static const std::string SG = "Green";
            inline static const std::string SY = "Yellow";

            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates a tomcat model object.
             */
            Tomcat();

            ~Tomcat();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------
            Tomcat(const Tomcat&) = default;

            Tomcat& operator=(const Tomcat&) = default;

            Tomcat(Tomcat&&) = default;

            Tomcat& operator=(Tomcat&&) = default;

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Initializes ToMCAT as the v1.0 of the model for the TA3 testbed
             * as a DBN.
             */
            void init_ta3_learnable_model();

            /**
             * Generates synthetic data for the instantiated model.
             *
             * @param random_generator: random number generator
             * @param num_samples: number of samples to generate
             * @param output_folder: folder where the samples must be saved
             * @param equal_until: max time step for equal samples. After this
             * time step, samples are not required to be the same.
             */
            void
            generate_synthetic_data(std::shared_ptr<gsl_rng> random_generator,
                                    int num_samples,
                                    const std::string& output_folder,
                                    int equal_until = -1);

            //------------------------------------------------------------------
            // Getters & Setters
            //------------------------------------------------------------------
            const std::shared_ptr<DynamicBayesNet>& get_model() const;

          private:
            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------
            std::shared_ptr<DynamicBayesNet> model;
        };

    } // namespace model
} // namespace tomcat
