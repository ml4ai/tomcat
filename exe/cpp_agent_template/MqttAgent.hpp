#pragma once

#include <iostream>
#include <boost/json.hpp>
#include <mqtt/async_client.h>
#include "Agent.hpp"
#include "ReferenceMessageHandler.hpp"
#include "HeartbeatProducer.hpp"

namespace json = boost::json;
using namespace std;


/** Agent class that manages MQTT traffic  */
class MqttAgent : public Agent {

    HeartbeatProducer heartbeat_producer = HeartbeatProducer(this);

    std::shared_ptr<mqtt::async_client> mqtt_client;

    json::stream_parser json_parser;

    bool running = false;

  public:

    void start();
    void stop();

    /** Constructor */
    MqttAgent(const json::object &config);

    /* send output to the message bus */
    void publish(const string topic, json::object &message) override;

    /* send output to the message bus */
    void publish(const string topic, const string text) override;
};
