#pragma once

#include <string>
#include <boost/json.hpp>
#include <mqtt/async_client.h>
#include "Utils.hpp"

namespace json = boost::json;
using namespace std;

class Coordinator;

/** A base class for subscribed message handlers */
class Processor{

    private:

    json::stream_parser json_parser;

    protected:

    // routines called by any class
    Utils utils;
    
    // publication source
    string source;

    // this software version
    string version;

    /** publication to the Message Bus */
    std::shared_ptr<mqtt::async_client> mqtt_client;

    void publish(json::value jv);

    public:

    // subscribed and published message fields
    Utils::Configuration input_config, output_config;

    /** used by this program only */
    virtual string get_subscription_name() { return ""; }
    virtual string get_publication_name() { return ""; }

    /** setup before running */
    void configure(
        json::object config,
        std::shared_ptr<mqtt::async_client> mqtt_client
    );

    /** Flag to specify whether the processor is running or not */
    bool running = true;
    virtual void start(){ running = true;}
    virtual void stop(){ running = false;}

    /** handle any message bus traffic */
    void process_message(string topic, mqtt::const_message_ptr msg);

    /** process a message read from the Message Bus */
    virtual void process_input_message(
        json::object input_header,
        json::object input_msg,
        json::object input_data
    ) {}

    /* compose a publication header based on subscribed header */
    json::value header(string timestamp, json::object input_header);

    /* compose a publication msg based on subscribed msg */
    json::value msg(string timestamp, json::object input_msg);
};
