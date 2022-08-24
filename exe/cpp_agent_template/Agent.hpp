#pragma once

#include <boost/json.hpp>
#include "ReferenceMessageHandler.hpp"
#include "Utils.hpp"

namespace json = boost::json;

using namespace std;


// interface for write method
class Agent : public Utils {

    // only process JSON messages
    json::stream_parser json_parser;
    
    ReferenceMessageHandler message_handler = ReferenceMessageHandler(this);

    protected:

    string app_name;

    void process_message(const json::object &message);
    void enqueue_message(const json::object &message);

    json::object parse_json(const string text);

    string version;

    Agent(const json::object &config);

    vector<string>get_input_topics();

    vector<string>get_output_topics();

    virtual void start();
    virtual void stop();

    public:

    virtual void publish(json::object &message) = 0;
};
