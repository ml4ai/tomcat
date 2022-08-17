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

    /** Agents for subscribed message topics */
    ReferenceAgent a0;
    VersionInfoAgent a1;
    RollcallAgent a2;
    HeartbeatAgent a3;
    static const int N_AGENTS = 4;
    BaseAgent *agents[N_AGENTS] = {&a0, &a1, &a2, &a3};

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
