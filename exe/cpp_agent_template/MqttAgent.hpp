#pragma once

#include <memory>
#include <string>
#include <thread>
#include <future>
#include <boost/json.hpp>

#include <mqtt/async_client.h>

#include "MessageHandler.hpp"
#include "ReferenceMessageHandler.hpp"
#include "TrialMessageHandler.hpp"
#include "RollcallMessageHandler.hpp"
#include "HeartbeatProducer.hpp"

namespace json = boost::json;

using namespace std;


/** Class that manages a MQTT broker connection and message handlers */
class MqttAgent {

    /** handlers for subscribed messages */
    ReferenceMessageHandler h0;
    TrialMessageHandler h1;
    RollcallMessageHandler h2;
    HeartbeatProducer h3;
    static const int N_MESSAGE_HANDLERS = 4;
    MessageHandler *message_handlers[N_MESSAGE_HANDLERS] = 
        {&h0, &h1, &h2, &h3};

    /** Flag to specify whether the agent is running or not */
    bool running = true;

    /** MQTT client */
    std::shared_ptr<mqtt::async_client> mqtt_client;

  public:

    /** Constructor */
    MqttAgent(json::object config);

    /** Stop the agent */
    void stop();

    /** Start the agent */
    void start();
};
