#pragma once

#include <chrono>

#include "Estimation.h"

namespace tomcat {
    namespace model {

        //------------------------------------------------------------------
        // Structs
        //------------------------------------------------------------------

        /**
         * This struct contains information needed to connect to a message
         * broker to either subscribe or publish to a topic.
         */
        struct MessageBrokerConfiguration {
            std::string address;
            std::string port;

            // If defined, the estimation thread will terminate after the number
            // of seconds here defined without receiving any message.
            int timeout = -1;

            // Topics to subscribe to
            std::string state_topic = "observations/state";
            std::string chat_topic = "observations/chat";
            std::string events_topic = "observations/events/#";
            std::string self_report_topic = "observations/self_reports";

            // Topics to publish to
            std::string estimation_topic = "estimations/xxxx";
        };

        /**
         * Class responsible for computing estimates for a model in an online
         * fashion. It listens to a message bus topic to compute estimates in
         * real time as data is observed.
         */
        class OnlineEstimation : public Estimation {
          public:
            //------------------------------------------------------------------
            // Types, Enums & Constants
            //------------------------------------------------------------------
            typedef std::chrono::steady_clock::time_point Time;

            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates an online estimation process.
             *
             * @param estimator: type of estimation to be performed
             */
            OnlineEstimation(std::shared_ptr<Estimator> estimator,
                             MessageBrokerConfiguration& config);

            /**
             * Creates an online estimation process.
             *
             * @param estimator: type of estimation to be performed
             */
            OnlineEstimation(std::shared_ptr<Estimator> estimator,
                             MessageBrokerConfiguration&& config);

            ~OnlineEstimation();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------
            OnlineEstimation(const OnlineEstimation& estimation);

            OnlineEstimation& operator=(const OnlineEstimation& estimation);

            OnlineEstimation(OnlineEstimation&&) = default;

            OnlineEstimation& operator=(OnlineEstimation&&) = default;

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------
            void estimate(EvidenceSet test_data) override;

          private:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Copies data members from another online estimation process.
             */
            void copy_estimation(const OnlineEstimation& estimation);

            // TODO - remove this later
            void reset() override;

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------
            MessageBrokerConfiguration config;

            // Last time the estimation process received a message.
            Time last_updated_time;

            // TODO - remove this later
            bool init = false;
        };

    } // namespace model
} // namespace tomcat
