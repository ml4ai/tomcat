#pragma once

#include <memory>
#include <string>
#include <boost/json.hpp>

#include <mqtt/async_client.h>

#include "Agent.hpp"

namespace json = boost::json;

using namespace std;


/** Agent class that manages MQTT traffic  */
class MqttAgent : public Agent {

    std::shared_ptr<mqtt::async_client> mqtt_client;

    json::stream_parser json_parser;

  public:

    /** Constructor */
    MqttAgent(json::object config);

    ~MqttAgent(){}

    /* send output to the message bus */
    void write(string topic, json::object message) override;
};
