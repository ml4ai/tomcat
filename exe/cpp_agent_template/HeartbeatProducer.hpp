#pragma once

#include <memory>
#include <string>
#include <thread>
#include <future>
#include <boost/json.hpp>

#include <mqtt/async_client.h>

#include "Processor.hpp"
#include "HeartbeatMessage.hpp"
#include "ReferenceAgentInputProcessor.hpp"
#include "RollcallRequestProcessor.hpp"
#include "TrialStartProcessor.hpp"
#include "TrialStopProcessor.hpp"
#include "Utils.hpp"

namespace json = boost::json;

using namespace std;


/** Class that handles general tasks */
class HeartbeatProducer {

    /** configuration */
    Utils::Configuration pub_config;

    /** config state */
    json::object config;

    // publication source
    string source;

    // This software version
    string version;

    /** publication to the Message Bus */
    std::shared_ptr<mqtt::async_client> mqtt_client;

    /** Flag to specify whether the agent is running or not */
    bool running = true;

    /** std::future object holds the result of the async heartbeat operation
     */
    std::future<void> heartbeat_future;

    /** Function that publishes heartbeat messages on an interval */
    void publish_heartbeats();

    /** get timestamp, etc */
    Utils utils;

    /** create outgoing JSON value for heartbeat */
    json::value get_heartbeat();

  public:

    /** Start producing heartbeats */
    void configure(
        json::object config, 
	std::shared_ptr<mqtt::async_client> mqtt_client
    );

    /** Stop the agent */
    void stop();
};
