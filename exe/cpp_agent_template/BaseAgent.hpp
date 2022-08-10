#pragma once

#include <memory>
#include <string>
#include <thread>
#include <future>
#include <boost/json.hpp>

#include <mqtt/async_client.h>

namespace json = boost::json;

using namespace std;


/** Class that handles general tasks */
class BaseAgent {

    /** configuration settings */
    json::object config;

    /** Heartbeats publication topic */
    string heartbeats_topic;

    /** Rollcall Response publication topic */
    string rollcall_response_topic;

    /** Version Info publication topic */
    string version_info_topic;

    /** Agent output publication topic */
    string agent_publication_topic;

    /** Flag to specify whether the agent is running or not */
    bool running = true;

    /** std::future object holds the result of the async heartbeat operation
     */
    std::future<void> heartbeat_future;

    /** Function that processes incoming messages */
    virtual void process(mqtt::const_message_ptr msg) {};

    /** Function that publishes heartbeat messages while the agent is running */
    void publish_heartbeats();

  private:
    void init_publishers(json::object config);

  public:
    std::shared_ptr<mqtt::async_client> mqtt_client;

    /** Constructor */
    BaseAgent(json::object config);

    /** Stop the agent */
    void stop();
};
