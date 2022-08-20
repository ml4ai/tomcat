#pragma once

#include <boost/json.hpp>
#include "MessageHandler.hpp"

namespace json = boost::json;

using namespace std;


class Agent {
    Agent(){}

    public:

    MessageHandler message_handler = MessageHandler(this);

    Agent(json::object config);
    ~Agent(){}
    

    // Stop the agent
    virtual void stop(){}

    // Start the agent
    virtual void start(){}

    // configure agents
    void configure(json::object config);

    // message handler input 
    void process_message(string topic, json::object message);

    // message handler output
    virtual void write(string topic, json::object message) = 0;
};
