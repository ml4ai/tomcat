#pragma once

#include <memory>
#include <thread>

#include <nlohmann/json.hpp>

#include "EstimationProcess.h"

#include "converter/MessageConverter.h"
#include "utils/Definitions.h"
#include "utils/Mosquitto.h"
#include "utils/SynchronizedQueue.h"

namespace tomcat {
    namespace model {

        /**
         * Class responsible for computing estimates for a model in an online
         * fashion. It listens to a message bus topic to compute estimates in
         * real time as data is observed.
         */
        class OnlineEstimation : public EstimationProcess, public Mosquitto {
          public:
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

                // If defined, the estimation thread will terminate after the
                // number of seconds here defined without receiving any message.
                int timeout = 9999;

                // Topics to subscribe to
                std::string state_topic = "observations/state";
                std::string chat_topic = "observations/chat";
                std::string events_topic = "observations/events/#";
                std::string self_report_topic = "observations/self_reports";
                std::string trial_topic = "trial";

                // Topics to publish to
                std::string estimates_topic = "tomcat/estimates";
                std::string log_topic = "tomcat/log";
            };

            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates an online estimation process.
             *
             * @param estimator: type of estimation to be performed
             * @param message_converter: responsible for converting a message
             * from the message bus to data
             */
            OnlineEstimation(
                MessageBrokerConfiguration config,
                std::shared_ptr<MessageConverter> message_converter);

            ~OnlineEstimation();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------
            OnlineEstimation(const OnlineEstimation& estimation);

            OnlineEstimation& operator=(const OnlineEstimation& estimation);

            // The synchronized queue used in this class has no move
            // constructor, so let's not allow this class to be moved as well..
            OnlineEstimation(OnlineEstimation&&) = delete;

            OnlineEstimation& operator=(OnlineEstimation&&) = delete;

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------
            void estimate(const EvidenceSet& test_data) override;

          protected:
            void prepare() override;

            void on_error(const std::string& error_message) override;

            void on_message(const std::string& topic,
                            const std::string& message) override;

            void on_time_out() override;

            void get_info(nlohmann::json& json) const override;

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
             * Returns next set of observations from the pending messages in the
             * queue.
             *
             * @return Evidence set.
             */
            EvidenceSet get_next_data_from_pending_messages();

            /**
             * Publishes last computed estimates to the message bus.
             */
            void publish_last_estimates();

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------
            MessageBrokerConfiguration config;

            std::shared_ptr<MessageConverter> message_converter;

            // Number of time steps the estimation already processed.
            int time_step;

            // Messages received from the message bus and stored to be processed
            // by the estimation threads.
            SynchronizedQueue<nlohmann::json> messages_to_process;
        };

    } // namespace model
} // namespace tomcat
