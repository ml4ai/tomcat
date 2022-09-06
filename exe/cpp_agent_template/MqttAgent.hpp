#pragma once

#include <boost/json.hpp>
#include <future>
#include <iostream>
#include <mqtt/async_client.h>
#include <thread>

#include "Agent.hpp"

namespace json = boost::json;

/** Agent class that manages MQTT traffic  */
class MqttAgent : public Agent {

    std::shared_ptr<mqtt::async_client> mqtt_client;

    bool running = true;
    bool busy = false;

    // Message queue gets checked once per second
    std::future<void> queue_future;
    void check_queue();

    // A FIFO queue of messages from the bus
    std::deque<json::object> message_queue;

  public:
    void start() override;
    void stop() override;

    /** Constructor */
    MqttAgent(const json::object& config);

    /* send output to the message bus */
    void publish(json::object& message) override;

    void process_next_message() override;
};
