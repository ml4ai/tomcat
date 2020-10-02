#pragma once

#include "utils/Definitions.h"
#include "pgm/Node.h"
#include "distribution/Distribution.h"

namespace tomcat {
    namespace model {

        /**
         * Abstract continuous probability distribution.
         */
        class Continuous : public Distribution {
          public:
            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates an abstract representation of a continuous probability
             * distribution.
             */
            Continuous();

            /**
             * Creates an abstract representation of a continuous probability
             * distribution for node dependent parameters.
             *
             * @param parameters: nodes which the assignments define the set
             * of parameters of the distribution
             */
            Continuous(std::vector<std::shared_ptr<Node>>& parameters);

            /**
             * Creates an abstract representation of a continuous probability
             * distribution for node dependent parameters.
             *
             * @param parameters: nodes which the assignments define the set
             * of parameters of the distribution
             */
            Continuous(std::vector<std::shared_ptr<Node>>&& parameters);

            virtual ~Continuous();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------

            // Copy constructor and assignment should be deleted to avoid
            // implicit slicing and loss of polymorphic behaviour in the
            // subclasses. To deep copy, the clone method must be used.
            Continuous(const Continuous&) = delete;

            Continuous& operator=(const Continuous&) = delete;

            Continuous(Continuous&&) = default;

            Continuous& operator=(Continuous&&) = default;

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            void update_sufficient_statistics(const Eigen::VectorXd& sample) override;

            Eigen::VectorXd get_values() const override;

          protected:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------
            void update_dependencies(Node::NodeMap& parameter_nodes_map,
                                     int time_step) override;

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------

            // The assignment of a node defines one of the parameters of the
            // distribution.
            std::vector<std::shared_ptr<Node>> parameters;
        };

    } // namespace model
} // namespace tomcat
