#pragma once

#include <memory>
#include <string>
#include <thread>
#include <future>
#include <boost/json.hpp>

#include <mqtt/async_client.h>

#include "BaseAgent.hpp"

namespace json = boost::json;

using namespace std;


/** Class that handles general tasks */
class HeartbeatAgent : public BaseAgent {
    public:

    json::object get_input_config(json::object config) override;

    json::object get_output_config(json::object config) override;

    json::object get_output_data(json::object input_data) override;


    void start() override;
    void stop() override;


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

};
