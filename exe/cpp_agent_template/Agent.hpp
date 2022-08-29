#pragma once

#include <iostream>
#include <boost/json.hpp>
#include "ReferenceMessageHandler.hpp"
#include "Utils.hpp"

namespace json = boost::json;

using namespace std;


// interface for write method
class Agent : public Utils {

    // only process JSON messages
    json::stream_parser json_parser;

    protected:

    string app_name;

    ReferenceMessageHandler message_handler = ReferenceMessageHandler(this);


    json::object parse_json(const string text);

    void configure(const json::object &config);

    virtual void start() {}
    virtual void stop() {}

    public:

    virtual void process_next_message() {}
    virtual void publish(json::object &message) {}
};
