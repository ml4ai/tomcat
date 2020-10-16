#pragma once

#include "TomcatTA3.h"

#include "utils/Definitions.h"

namespace tomcat {
    namespace model {

        /**
         * ToMCAT V2.0 PGM
         */
        class TomcatTA3V2 : public TomcatTA3 {
          public:
            inline static const int NUM_TRAINING_CONDITIONS = 3;
            inline static const int NUM_BEEP_STATES = 3;

            // Hidden variable labels
            inline static const std::string THETA_Q_PRIOR = "Theta_Q_Prior";
            inline static const std::string THETA_PBAE_PRIOR = "Theta_PBAE_Prior";
            inline static const std::string THETA_PBAE = "Theta_PBAE"; // Transition
            inline static const std::string PBAE = "PBAE";
            inline static const std::string Q = TA3MessageConverter::Q;
            inline static const std::string BEEP = TA3MessageConverter::BEEP;

            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates a tomcat v2.0 model object.
             */
            TomcatTA3V2();

            ~TomcatTA3V2();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------
            TomcatTA3V2(const TomcatTA3V2& tomcat);

            TomcatTA3V2& operator=(const TomcatTA3V2& tomcat);

            TomcatTA3V2(TomcatTA3V2&&) = default;

            TomcatTA3V2& operator=(TomcatTA3V2&&) = default;

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Initializes v2.0 of the ToMCAT model for the TA3 testbed
             * as a DBN.
             */
            void init() override;

          protected:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Defines ToMCAT nodes from v1.0 of the model for the TA3 testbed.
             */
            std::unordered_map<std::string, std::shared_ptr<RandomVariableNode>>
            get_nodes() const override;

            /**
             * Creates metadata for the node Theta_S
             *
             * @return Node metadata.
             */
            std::vector<std::shared_ptr<NodeMetadata>>
            create_theta_s_metadatas() const override;

            /**
             * Creates prior distribution for the parameter nodes Theta_S.
             *
             * @return CPD
             */
            std::vector<std::shared_ptr<CPD>>
            create_theta_s_prior_cpds() const override;

            /**
             * Creates metadata for the parameter node Theta_Q_Prior.
             *
             * @return Node metadata.
             */
            std::shared_ptr<NodeMetadata> create_theta_q_prior_metadata() const;

            /**
             * Creates metadata for the parameter node Theta_PBAE_Prior.
             *
             * @return Node metadata.
             */
            std::shared_ptr<NodeMetadata>
            create_theta_pbae_prior_metadata() const;

            /**
             * Creates metadatas for the parameter nodes Theta_PBAE.
             *
             * @return Node metadata.
             */
            std::vector<std::shared_ptr<NodeMetadata>>
            create_theta_pbae_metadatas() const;

            /**
             * Creates metadata for the node Q.
             *
             * @return Node metadata.
             */
            std::shared_ptr<NodeMetadata>
            create_training_condition_metadata() const;

            /**
             * Creates metadata for the node Player's Belief About the
             * Environment (PBAE).
             *
             * @return Node metadata.
             */
            std::shared_ptr<NodeMetadata> create_pbae_metadata() const;

            /**
             * Creates metadata for the node Beep.
             *
             * @return Node metadata.
             */
            std::shared_ptr<NodeMetadata> create_beep_metadata() const;

            /**
             * Creates prior distribution for the parameter node Theta_Q.
             *
             * @return CPD
             */
            std::shared_ptr<CPD> create_theta_q_prior_cpd() const;

            /**
             * Creates prior distribution for the parameter node Theta_PBAE_Prior.
             *
             * @return CPD
             */
            std::shared_ptr<CPD> create_theta_pbae_prior_prior_cpd() const;

            /**
             * Creates prior distributions for the parameter nodes Theta_PBAE.
             *
             * @return CPD
             */
            std::vector<std::shared_ptr<CPD>> create_theta_pbae_prior_cpds() const;

            /**
             * Creates prior distribution for the node Q.
             *
             * @param theta_q_node: parameter node Theta_Q
             *
             * @return CPD
             */
            std::shared_ptr<CPD> create_training_condition_prior_cpd(
                std::shared_ptr<RandomVariableNode> theta_q_node) const;

            /**
             * Creates prior distribution for the node Player's Belief About the
             * Environment (PBAE).
             *
             * @param theta_pbae_node: parameter node Theta_PBAE
             *
             * @return CPD
             */
            std::shared_ptr<CPD> create_pbae_prior_cpd(
                std::shared_ptr<RandomVariableNode> theta_pbae_node) const;

            /**
             * Creates prior distributions for the node Beep given the current
             * Player Belief About the Environment.
             *
             * @param pbae_metadata: metadata of the node PBAE
             *
             * @return CPD
             */
            std::shared_ptr<CPD>
            create_beep_cpd(std::shared_ptr<NodeMetadata> pbae_metadata) const;
        };

    } // namespace model
} // namespace tomcat
