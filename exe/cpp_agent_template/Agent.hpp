#pragma once

#include <memory>
#include <string>
#include <thread>
#include <future>

#include <mqtt/async_client.h>

using namespace std;


/** Class that represents our agent */
class Agent {
    std::shared_ptr<mqtt::async_client> mqtt_client;

    /** Flag to specify whether the agent is running or not */
    bool running = true;

    /** std::future object to hold the result of the async heartbeat operation
     */
    std::future<void> heartbeat_future;

    /** Function that processes incoming messages */
    virtual void process(mqtt::const_message_ptr msg) {};

    /** Function that publishes heartbeat messages while the agent is running */
    void publish_heartbeats();


  public:
    /** Constructor */
    Agent(string address, string input_topic, string output_topic);

    /** Stop the agent */
    void stop();
};
