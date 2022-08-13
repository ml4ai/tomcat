#pragma once

#include <string>
#include <boost/json.hpp>
#include <mqtt/async_client.h>
#include "Message.hpp"
#include "Utils.hpp"

namespace json = boost::json;
using namespace std;


/** A base class for subscribed message handlers */
class Processor{

    public:
    Utils utils;

    // for reading from the message bus
    Utils::Configuration sub_config;

    // for writing from the message bus
    Utils::Configuration pub_config;

    json::stream_parser json_parser;

    /** input/output IDs */
    virtual string get_subscription_name() = 0;
    virtual string get_publication_name() = 0;

    /** message bus traffic */
    void process(mqtt::const_message_ptr msg);

    /** set fields needed to identify messages to process */
    void configure(json::object config);

    /** process known good message json */
    virtual void process(
        json::object common_header,
        json::object common_message,
        json::object data
    ) = 0;
};
