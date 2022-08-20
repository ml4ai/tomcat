#pragma once

#include <boost/json.hpp>
#include "ReferenceMessageHandler.hpp"

namespace json = boost::json;

using namespace std;


class Agent {

    ReferenceMessageHandler message_handler;

    public:

    Agent(const json::object &config);
    ~Agent(){}

    // start the agent
    virtual void start() {}

    // stop the agent
    virtual void stop() {}

    // input
    vector<string> get_read_topics();
    void read(const string topic, const json::object &message);

    // output
    vector<string> get_write_topics();
    virtual void write(const string topic, json::object &message) {}
};
