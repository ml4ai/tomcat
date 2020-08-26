#pragma once

#include <chrono>

#include "Estimation.h"

#include "../../utils/Definitions.h"
#include "../../utils/Mosquitto.h"

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
            int port;

            // If defined, the estimation thread will terminate after the number
            // of seconds here defined without receiving any message.
            int timeout = 9999;

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
        class OnlineEstimation : public Estimation, public Mosquitto {
          public:
            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates an online estimation process.
             *
             * @param estimator: type of estimation to be performed
             */
            OnlineEstimation(std::shared_ptr<Estimator> estimator,
                             MessageBrokerConfiguration config);

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

          protected:
            void reset() override;

            void on_error(const std::string& error_message) override;

            void on_message(const std::string& topic,
                            const std::string& message) override;

            void on_time_out() override;

          private:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Copies data members from another online estimation process.
             */
            void copy_estimation(const OnlineEstimation& estimation);

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------
            MessageBrokerConfiguration config;

            // Number of time steps the estimation already processed.
            int time_step;
        };

    } // namespace model
} // namespace tomcat
