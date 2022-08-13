#pragma once

#include <string>
#include <boost/json.hpp>
#include <mqtt/async_client.h>
#include "Message.hpp"
#include "Utils.hpp"

namespace json = boost::json;
using namespace std;

class Coordinator;

/** A base class for subscribed message handlers */
class Processor{

    private:

    Utils utils;
    json::stream_parser json_parser;

    protected:
    
    // publication source
    string source;

    // this software version
    string version;

    Coordinator *coordinator;

    void publish(json::value message);

    public:

    // subscribed and published message fields
    Utils::Configuration pub_config, sub_config;

    /** used by this program only */
    virtual string get_subscription_name() = 0;
    virtual string get_publication_name() = 0;

    /** set fields needed to identify messages to process */
    void configure(Coordinator* coordinator, json::object config);

    /** handle any message bus traffic */
    void process_traffic(string topic, mqtt::const_message_ptr msg);

    /** process a message directed at us */
    virtual void process_subscribed_message(
        json::object sub_header,
        json::object sub_msg,
        json::object sub_data
    ) = 0;


    /* compose a publication header based on subscribed header */
    json::value header(string timestamp, json::object sub_header);

    /* compose a publication msg based on subscribed msg */
    json::value msg(string timestamp, json::object sub_msg);
};
