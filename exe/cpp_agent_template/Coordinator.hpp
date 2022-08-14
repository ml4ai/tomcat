#pragma once

#include <memory>
#include <string>
#include <thread>
#include <future>
#include <boost/json.hpp>

#include <mqtt/async_client.h>

#include "Processor.hpp"
#include "HeartbeatProducer.hpp"
#include "ReferenceAgentProcessor.hpp"
#include "RollcallRequestProcessor.hpp"
#include "TrialStartProcessor.hpp"
#include "TrialStopProcessor.hpp"
#include "Utils.hpp"

namespace json = boost::json;

using namespace std;


/** Class that handles general tasks */
class Coordinator {

    /** Processors for subscribed message topics */
    TrialStartProcessor p0;
    TrialStopProcessor p1;
    ReferenceAgentProcessor p2;
    RollcallRequestProcessor p3;
    static const int N_PROCESSORS = 4;
    Processor *processors[N_PROCESSORS] = {&p0, &p1, &p2, &p3};

    /** general functions */
    Utils utils;

    /** config state */
    json::object config;

    /** Flag to specify whether the agent is running or not */
    bool running = true;

    /** Function that processes incoming messages */
    virtual void process(mqtt::const_message_ptr msg) {};

    /** heartbeats every 10 s */
    HeartbeatProducer heartbeat_producer;

  public:
    std::shared_ptr<mqtt::async_client> mqtt_client;

    /** Constructor */
    Coordinator(json::object config);

    /** Stop the agent */
    void stop();
};
