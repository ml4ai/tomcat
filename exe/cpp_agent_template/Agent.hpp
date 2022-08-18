#pragma once

#include <boost/json.hpp>
#include "MessageHandler.hpp"

namespace json = boost::json;

using namespace std;


class Agent {

    public:

    MessageHandler message_handler;

    Agent(json::object config);
    Agent(){}
    ~Agent(){}
    

    // Stop the agent
    void stop();

    // Start the agent
    void start();

    // configure agents
    void configure(json::object config);

    // return the set of configured input topics
    vector<string> get_input_topics();

    // return the set of configured output topics
    vector<string> get_output_topics();

    // message handler input 
    void process_message(string topic, json::object message);

    // message handler output
    virtual void write(string topic, json::object message) = 0;
};
