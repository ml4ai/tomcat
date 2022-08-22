#pragma once

#include <memory>
#include <string>
#include <thread>
#include <future>
#include <boost/json.hpp>

#include "BaseMessageHandler.hpp"

namespace json = boost::json;

using namespace std;

class Agent;


// publications
#define HEARTBEAT_TOPIC "status/reference_agent/heartbeats"
#define HEARTBEAT_MESSAGE_TYPE "status"
#define HEARTBEAT_SUB_TYPE "heartbeat"


/** Class that handles general tasks */
class HeartbeatProducer : public BaseMessageHandler {

    // keep the data element so we don't need to constantly recreate it
    json::object output_data = json::object();
    
    /** because this class does not respond to messages directly,
     *  we must keep a copy of the most recent subscribed message */
    json::object input_message = json::object();

    // holds the result of the async heartbeat operation
    std::future<void> heartbeat_future;

    /** Publishe heartbeat messages on an interval */
    void publish_heartbeats();
    void publish_heartbeat();

    // produce heartbeats when true
    bool running = false;


    public:

    HeartbeatProducer(Agent *agent): BaseMessageHandler(agent){}

    virtual void configure(const json::object &config) override;

    virtual void process_message(const string topic,
        const json::object &message) override;

    void start();
    void stop();
};
