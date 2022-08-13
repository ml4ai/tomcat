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

namespace json = boost::json;

using namespace std;


/** Class that handles general tasks */
class Coordinator {

    /** Processors for subscribed message topics */
    TrialStartProcessor p0;
    TrialStopProcessor p1;
    ReferenceAgentInputProcessor p2;
    RollcallRequestProcessor p3;
    static const int N_PROCESSORS = 4;
    Processor *processors[N_PROCESSORS] = {&p0, &p1, &p2, &p3};

    /** published heartbeat */
    HeartbeatMessage *heartbeat_message = nullptr;

    /** config state */
    json::object config;

    /** Flag to specify whether the agent is running or not */
    bool running = true;

    /** std::future object holds the result of the async heartbeat operation
     */
    std::future<void> heartbeat_future;

    /** Function that processes incoming messages */
    virtual void process(mqtt::const_message_ptr msg) {};

    /** Function that publishes heartbeat messages on an interval */
    void publish_heartbeats();


  public:
    std::shared_ptr<mqtt::async_client> mqtt_client;

    /** publisher */
    void publish(string topic, json::value message);

    /** Destructor */
    ~Coordinator();

    /** Constructor */
    Coordinator(json::object config);

    /** Stop the agent */
    void stop();
};
