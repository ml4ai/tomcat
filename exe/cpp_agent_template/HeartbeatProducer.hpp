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
    
    json::object output_data;

    public:

    HeartbeatProducer();
    ~HeartbeatProducer(){}

    void start() override;

    void stop() override;

    string get_input_config_name() override { return "trial_start";}

    string get_output_config_name() override { return "heartbeat";}

    json::object create_output_data(json::object input_data) override;

    void process_json_message(json::object json_message) override;

    void set_status(string state, bool active, string status);

    /** because this class does not respond to messages directly,
     *  we must keep a copy of a passed-in message */
    json::object input_header;
    json::object input_msg;

    /** std::future object holds the result of the async heartbeat operation
     */
    std::future<void> heartbeat_future;

    /** Publishe heartbeat messages on an interval */
    void publish_heartbeats();

    /** Publishes a heartbeat message immediately */
    void publish_heartbeat();

    /** create outgoing JSON value for heartbeat */
    json::object get_heartbeat();
};
