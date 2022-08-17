#pragma once

#include <memory>
#include <string>
#include <thread>
#include <future>
#include <boost/json.hpp>

#include <mqtt/async_client.h>

#include "MessageHandler.hpp"

namespace json = boost::json;

using namespace std;


/** Class that handles general tasks */
class HeartbeatProducer : public MessageHandler {
    
    json::object data;

    /** because this class does not respond to messages directly,
     *  we must keep a copy of the most recent subscribed message */
    json::object input_message;

    // holds the result of the async heartbeat operation
    std::future<void> heartbeat_future;

    /** Publishe heartbeat messages on an interval */
    void publish_heartbeats();

    protected:

    json::object get_data(json::object input_data) override { return data; }
    string get_input_config_name() override { return "trial_start";}
    string get_output_config_name() override { return "heartbeat";}
    void process_message(json::object input_message) override;

    // no branch on sub type
    bool valid_input_msg(json::object input_msg) override { return true;}


    public:

    void start() override;
    void stop() override;
    void configure(
        json::object config,
        std::shared_ptr<mqtt::async_client> mqtt_client
    ) override;
    void set_status(string state, bool active, string status);
};
