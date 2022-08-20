#pragma once

#include <iostream>
#include <boost/json.hpp>
#include <mqtt/async_client.h>
#include "Agent.hpp"
#include "MessageHandler.hpp"

namespace json = boost::json;
using namespace std;


/** Agent class that manages MQTT traffic  */
class MqttAgent : public Agent {

    std::shared_ptr<mqtt::async_client> mqtt_client;

    json::stream_parser json_parser;

  public:

    void start() override;
    void stop() override;

    /** Constructor */
    MqttAgent(const json::object &config);

    ~MqttAgent(){}

    /* send output to the message bus */
    void write(const string topic, json::object &message) override;
};
