#pragma once

#include <iostream>
#include <boost/json.hpp>
#include <mqtt/async_client.h>
#include "Agent.hpp"
#include "ReferenceMessageHandler.hpp"

namespace json = boost::json;
using namespace std;


/** Agent class that manages MQTT traffic  */
class MqttAgent : public Agent {

    std::shared_ptr<mqtt::async_client> mqtt_client;

    bool running = true;

  public:

    void start() override ;
    void stop() override ;

    /** Constructor */
    MqttAgent(const json::object &config);

    /* send output to the message bus */
    void publish(json::object &message) override;
};
