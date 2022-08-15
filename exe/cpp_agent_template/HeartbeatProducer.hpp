#pragma once

#include <memory>
#include <string>
#include <thread>
#include <future>
#include <boost/json.hpp>

#include <mqtt/async_client.h>

#include "Processor.hpp"

namespace json = boost::json;

using namespace std;


/** Class that handles general tasks */
class HeartbeatProducer : public Processor {

    public:

    string get_subscription_name() override {return "trial_start";}
    string get_publication_name() override {return "heartbeat";}

    void start() override;
    void stop() override;

    void process_input_message(
        json::object input_header,
        json::object input_msg,
        json::object input_data
    ) override;

    void set_input(json::object input_header, json::object input_msg);

    /** because this class does not respond to messages directly,
     *  we must keep a copy of a passed-in message */
    json::object input_header, input_msg;

    /** std::future object holds the result of the async heartbeat operation
     */
    std::future<void> heartbeat_future;

    /** Function that publishes heartbeat messages on an interval */
    void publish_heartbeats();

    /** create outgoing JSON value for heartbeat */
    json::value get_heartbeat();

  public:
};
