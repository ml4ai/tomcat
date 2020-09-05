#pragma once

#include <memory>
#include <string>

#include <gsl/gsl_rng.h>

#include "pgm/DynamicBayesNet.h"
#include "utils/Definitions.h"

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

            Tomcat();

            ~Tomcat();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------

            // Copy constructor and assignment should be deleted to avoid
            // implicit slicing and loss of polymorphic behaviour in the
            // subclasses. To deep copy, the clone method must be used.
            Tomcat(const Tomcat&) = delete;

            Tomcat& operator=(const Tomcat&) = delete;

            Tomcat(Tomcat&&) = default;

            Tomcat& operator=(Tomcat&&) = default;

            //------------------------------------------------------------------
            // Operator overload
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Static functions
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------
            void init_ta3_learnable_model();

            void
            generate_synthetic_data(std::shared_ptr<gsl_rng> random_generator,
                                    int num_samples,
                                    const std::string& output_folder);

            //------------------------------------------------------------------
            // Virtual functions
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Pure virtual functions
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Getters & Setters
            //------------------------------------------------------------------
            const std::shared_ptr<DynamicBayesNet>& get_model() const;

          protected:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------

          private:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------
            std::shared_ptr<DynamicBayesNet> model;
        };

    } // namespace model
} // namespace tomcat
