#pragma once

#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include <gsl/gsl_rng.h>

#include "model/pgm/DynamicBayesNet.h"
#include "model/pgm/NodeMetadata.h"
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
            inline static const std::string THETA_Q = "Theta_Q";
            inline static const std::string THETA_S = "Theta_S";
            inline static const std::string PI_RM = "Pi_RM";
            inline static const std::string PI_SG = "Pi_SG";
            inline static const std::string PI_SY = "Pi_SY";
            inline static const std::string Q = "Training_Condition";
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

            // Copy constructor and assignment should be deleted to avoid
            // implicit slicing and loss of polymorphic behaviour in the
            // subclasses. To deep copy, the clone method must be used.
            Tomcat(const Tomcat&) = delete;

            Tomcat& operator=(const Tomcat&) = delete;

            Tomcat(Tomcat&&) = default;

            Tomcat& operator=(Tomcat&&) = default;

            //------------------------------------------------------------------
            // Pure virtual functions
            //------------------------------------------------------------------

            /**
             * Creates unrolled DBN for the model.
             */
            virtual void init() = 0;

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Generates synthetic data for the instantiated model.
             *
             * @param random_generator: random number generator
             * @param num_samples: number of samples to generate
             * @param output_folder: folder where the samples must be saved
             * @param equal_until: max time step for equal samples. After this
             * time step, samples are not required to be the same.
             * @param max_time_step: generate data up to this time step.
             */
            void
            generate_synthetic_data(std::shared_ptr<gsl_rng> random_generator,
                                    int num_samples,
                                    const std::string& output_folder,
                                    int equal_until = -1,
                                    int max_time_step = -1);

            //------------------------------------------------------------------
            // Getters & Setters
            //------------------------------------------------------------------
            const std::shared_ptr<DynamicBayesNet>& get_model() const;

          protected:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /*
             * Copies the data members from another ToMCAT's instance
             */
            void copy_tomcat(const Tomcat& tomcat);

            /**
             * Create a random variable node for a given metadata and list of
             * CPDs.
             *
             * @return Random variable node.
             */
            std::shared_ptr<RandomVariableNode>
            create_node(std::shared_ptr<NodeMetadata> metadata,
                        std::vector<std::shared_ptr<CPD>> cpds) const;

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------
            std::shared_ptr<DynamicBayesNet> model;
        };

    } // namespace model
} // namespace tomcat
