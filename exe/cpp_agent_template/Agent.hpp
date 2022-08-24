#pragma once

#include <boost/json.hpp>
#include "ReferenceMessageHandler.hpp"

namespace json = boost::json;

using namespace std;


// interface for write method
class Agent {

    // this application class only processes JSON messages
    json::stream_parser json_parser;
    
    ReferenceMessageHandler message_handler = ReferenceMessageHandler(this);

    protected:

    void process_message(const string topic, const json::object &message);

    json::object parse_json(const string text);

    string version;

    Agent(const json::object &config);

    vector<string>get_input_topics();

    vector<string>get_output_topics();

    virtual void start();
    virtual void stop();

    public:


    virtual void publish(const string topic, json::object &message) = 0;
    virtual void publish(const string topic, const string text) = 0;
};
