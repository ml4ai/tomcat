#pragma once

#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include "Tomcat.h"

#include "pgm/DynamicBayesNet.h"
#include "pgm/NodeMetadata.h"
#include "utils/Definitions.h"
#include "converter/TA3MessageConverter.h"

namespace tomcat {
    namespace model {

        /**
         * ToMCAT V1.0 PGM
         */
        class TomcatTA3 : public Tomcat {
          public:
            //------------------------------------------------------------------
            // Types, Enums & Constants
            //------------------------------------------------------------------
            inline static const int NUM_STATES = 4;
            inline static const int NUM_SECONDS = 600;

            // Hidden variable labels
            inline static const std::string THETA_S = "Theta_S";
            inline static const std::string PI_RM = "Pi_RM";
            inline static const std::string PI_SG = "Pi_SG";
            inline static const std::string PI_SY = "Pi_SY";
            inline static const std::string STATE = "State";
            inline static const std::string ROOM = TA3MessageConverter::ROOM;
            inline static const std::string SG = TA3MessageConverter::SG;
            inline static const std::string SY = TA3MessageConverter::SY;

            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates a tomcat v1.0 model object.
             */
            TomcatTA3();

            ~TomcatTA3();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------
            TomcatTA3(const TomcatTA3& tomcat);

            TomcatTA3& operator=(const TomcatTA3& tomcat);

            TomcatTA3(TomcatTA3&&) = default;

            TomcatTA3& operator=(TomcatTA3&&) = default;

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Initializes v2.0 of the ToMCAT model for the TA3 testbed.
             */
            void init() override;

          protected:
            //------------------------------------------------------------------
            // Virtual functions
            //------------------------------------------------------------------

            /**
             * Defines ToMCAT nodes from v1.0 of the model for the TA3 testbed.
             */
            virtual std::unordered_map<std::string,
                                       std::shared_ptr<RandomVariableNode>>
            get_nodes() const;

            /**
             * Creates metadata for the node Theta_S
             *
             * @return Node metadata.
             */
            virtual std::vector<std::shared_ptr<NodeMetadata>>
            create_theta_s_metadatas() const;

            /**
             * Creates metadata for the node State
             *
             * @return Node metadata.
             */
            virtual std::shared_ptr<NodeMetadata> create_state_metadata() const;

            /**
             * Creates metadata for the node Room
             *
             * @return Node metadata.
             */
            virtual std::shared_ptr<NodeMetadata> create_room_metadata() const;

            /**
             * Creates metadata for the node SG
             *
             * @return Node metadata.
             */
            virtual std::shared_ptr<NodeMetadata> create_sg_metadata() const;

            /**
             * Creates metadata for the node SY
             *
             * @return Node metadata.
             */
            virtual std::shared_ptr<NodeMetadata> create_sy_metadata() const;

            /**
             * Creates prior distribution for the parameter nodes Theta_S.
             *
             * @return CPD
             */
            virtual std::vector<std::shared_ptr<CPD>>
            create_theta_s_prior_cpds() const;

            /**
             * Creates prior distribution for the node State.
             *
             * @return CPD
             */
            virtual std::shared_ptr<CPD> create_state_prior_cpd() const;

            /**
             * Creates distribution for the node State given the previous State.
             *
             * @return CPD
             */
            virtual std::shared_ptr<CPD> create_state_transition_cpd(
                std::vector<std::shared_ptr<NodeMetadata>>
                    parent_nodes_metadata,
                std::vector<std::shared_ptr<RandomVariableNode>> theta_s_nodes)
                const;

            /**
             * Creates prior distributions for the node Room given the current
             * State.
             *
             * @return CPD
             */
            virtual std::shared_ptr<CPD>
            create_room_cpd(std::shared_ptr<NodeMetadata> state_metadata) const;

            /**
             * Creates prior distribution for the node SG given the current
             * State
             *
             * @return CPD
             */
            virtual std::shared_ptr<CPD>
            create_sg_cpd(std::shared_ptr<NodeMetadata> state_metadata) const;

            /**
             * Creates prior distribution for the node SY given the current
             * State
             *
             * @return CPD
             */
            virtual std::shared_ptr<CPD>
            create_sy_cpd(std::shared_ptr<NodeMetadata> state_metadata) const;
        };

    } // namespace model
} // namespace tomcat
