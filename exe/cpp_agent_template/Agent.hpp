#pragma once

#include <memory>
#include <string>
#include <thread>
#include <future>

#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/json.hpp>
#include <boost/log/trivial.hpp>

#include <mqtt/async_client.h>

/** Class that represents our agent/AC */
class Agent {
    std::shared_ptr<mqtt::async_client> mqtt_client;
    //std::thread heartbeat_publisher;
    bool running = true;

    std::future<void> _future;

    /** Disconnect from the MQTT broker */
    void disconnect();

    /** Function that processes incoming messages */
    virtual void process(mqtt::const_message_ptr msg) {};

    /** Function that publishes heartbeat messages while the agent is running */
    void publish_heartbeats();

  public:
    Agent(std::string address);

    /** Destructor for the class that cleans up threads and disconnects from
     * the broker. */
    ~Agent();
};
