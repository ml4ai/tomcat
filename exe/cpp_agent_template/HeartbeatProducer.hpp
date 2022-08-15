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
    private:

    // input
    json::object trial_start_config, trial_stop_config;
    string trial_topic;

    // output
    json::object heartbeat_config;
    json::object version_info_config;
    string heartbeat_topic;
    string heartbeat_message_type;
    string heartbeat_sub_type;


    public:
    void start() override;
    void stop() override;

    void process_input_message(string topic,json::object message) override;

    void configure(json::object config) override;

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
