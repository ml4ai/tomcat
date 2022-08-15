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

    /** configuration read from file */
    json::object config;

    void publish(string topic, json::value jv);

    public:

    /** used by this program only */
    virtual string get_subscription_name() { return ""; }
    virtual string get_publication_name() { return ""; }

    virtual void configure(json::object config){};

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

    virtual void process_input_message(
        string topic, 
        json::object input_message
    ) {}

    /* compose output header based on input header */
    json::value header(
        string timestamp,
        string output_message_type,
        json::object input_header
    );

    /* compose output header based on input header */
    json::object header(
        string timestamp,
        json::object output_config,
        json::object input_header
    );

    /* compose output msg based on input msg */
    json::value msg(
        string timestamp,
        string output_sub_type,
        json::object input_msg
    );

    /* compose output msg based on input msg */
    json::value msg(
        string timestamp,
        json::object output_config,
        json::object input_msg
    );
};
