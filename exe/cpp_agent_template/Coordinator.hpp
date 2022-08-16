#pragma once

#include <memory>
#include <string>
#include <thread>
#include <future>
#include <boost/json.hpp>

#include <mqtt/async_client.h>

#include "BaseAgent.hpp"
#include "ReferenceAgent.hpp"
#include "VersionInfoAgent.hpp"
#include "RollcallAgent.hpp"
#include "HeartbeatAgent.hpp"

namespace json = boost::json;

using namespace std;


/** Class that handles general tasks */
class Coordinator {

    /** Processors for subscribed message topics */
    ReferenceAgent p0;
    VersionInfoAgent p1;
    RollcallAgent p2;
    HeartbeatAgent heartbeat;
    static const int N_AGENTS = 4;
    BaseAgent *agents[N_AGENTS] = {&p0, &p1, &p2, &heartbeat};

    /** config state */
    json::object config;

    /** Flag to specify whether the agent is running or not */
    bool running = true;

    /** MQTT client */
    std::shared_ptr<mqtt::async_client> mqtt_client;

  public:

    /** Constructor */
    Coordinator(json::object config);

    /** Stop the agent */
    void stop();

    /** Start the agent */
    void start();
};
