#pragma once

#include <boost/json.hpp>
#include <future>
#include <iostream>
#include <mqtt/async_client.h>
#include <thread>

#include "Agent.hpp"
#include "Processor.hpp"

namespace json = boost::json;

// Agent class that manages MQTT traffic
class MqttAgent : public Agent {

    std::shared_ptr<mqtt::async_client> mqtt_client;

    bool running = true;
    bool busy = false;

    // Message queue gets checked once per second
    std::future<void> queue_future;
    void check_queue();

    // A FIFO queue of messages from the Message Bus
    std::deque<json::object> message_queue;

  public:
    void start();
    void stop();

    // Constructor 
    MqttAgent(const json::object& config, Processor &processor);

    // write a message to the Message Bus 
    void publish(const std::string topic, 
                 const json::object& message) override;

    void process_next_message() override;
};
