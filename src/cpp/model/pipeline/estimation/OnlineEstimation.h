#pragma once

#include <thread>
#include <queue>

#include "EstimationProcess.h"

#include "model/utils/Definitions.h"
#include "model/utils/Mosquitto.h"

namespace tomcat {
    namespace model {

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
            std::string estimates_topic = "tomcat/estimates";
            std::string log_topic = "tomcat/log";
        };

        /**
         * Class responsible for computing estimates for a model in an online
         * fashion. It listens to a message bus topic to compute estimates in
         * real time as data is observed.
         */
        class OnlineEstimation : public EstimationProcess, public Mosquitto {
          public:
            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates an online estimation process.
             *
             * @param estimator: type of estimation to be performed
             */
            OnlineEstimation(MessageBrokerConfiguration config);

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

            /**
             * Function executed by a thread responsible for calculating the
             * estimates for a single estimator.
             *
             * @param estimator: estimator
             * @param test_data: data to estimate values over
             */
            void run_estimation_thread();

            /**
             * Publishes last computed estimates to the message bus.
             */
            void publish_last_estimates();

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------
            MessageBrokerConfiguration config;

            // Number of time steps the estimation already processed.
            int time_step;

            // Data received from the message bus and stored to be processed by
            // the estimation threads.
            std::queue<EvidenceSet> data_to_process;
        };

    } // namespace model
} // namespace tomcat
