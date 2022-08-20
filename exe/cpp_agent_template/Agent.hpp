#pragma once

#include <boost/json.hpp>
#include "MessageHandler.hpp"

namespace json = boost::json;

using namespace std;


class Agent {

    protected:

    MessageHandler message_handler;

    public:

    Agent(const json::object &config);
    ~Agent(){}

    // start the agent
    virtual void start() {}

    // stop the agent
    virtual void stop() {}

    // output dispatch 
    virtual void write(const string topic, json::object &message) {}
};
