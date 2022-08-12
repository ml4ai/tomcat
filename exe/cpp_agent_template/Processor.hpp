#pragma once

#include <string>
#include <boost/json.hpp>
#include <mqtt/async_client.h>
#include "Message.hpp"

namespace json = boost::json;
using namespace std;


/** A base class for subscribed message handlers */
class Processor{

    private:

    json::stream_parser json_parser;

    bool test_key_value(json::object obj, string key, string value);

    public:

    string message_type = "not set";   // header.message_type
    string sub_type = "not set";   // msg.sub_type
    string version = "not set";  // this software version, from config file
    string topic = "not set";   // message bus topic


    /** Processor ID */
    virtual string get_name() = 0;

    /** message bus traffic */
    void process(mqtt::const_message_ptr msg);

    /** set fields needed to identify messages to process */
    void configure(json::object config);

    /** process known good message json */
    virtual void process(json::object read_from_bus) = 0;
};
