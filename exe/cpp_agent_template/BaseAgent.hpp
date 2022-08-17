#pragma once

#include <string>
#include <boost/json.hpp>
#include <mqtt/async_client.h>

namespace json = boost::json;
using namespace std;

class Coordinator;

// A base class for subscribed message handlers
class BaseAgent {

    private:

    json::stream_parser json_parser;

    protected:
    // subscription
    string input_topic;
    string input_message_type;
    string input_sub_type;

    // publication
    string output_topic;
    string output_message_type;
    string output_sub_type;

    // this software publication msg.source field
    string source;

    // this software version
    string version;

    // publication to the Message Bus
    std::shared_ptr<mqtt::async_client> mqtt_client;

    // configuration data for all agents
    json::object config;

    // publish value on output_topic
    void publish(json::value jv);

    // header object for output message
    json::object create_output_header(
        string timestamp,
       	json::object input_header
    );

    // timestamps
    string get_timestamp();

    // msg object for output message
    json::object create_output_msg(string timestamp, json::object input_msg);

    // agent-specific data object for output message
    virtual json::object create_output_data(json::object input_data);

    // get Message Bus subscription parameters
    virtual json::object get_input_config(json::object config);

    // get Message Bus publication parameters 
    virtual json::object get_output_config(json::object config);

    // called with the parsed MQTT message payload
    virtual void process_json_message(json::object json_message);

    // create an output message based on the input
    json::object create_output_message(
        json::object input_header,
        json::object input_msg,
        json::object input_data
    );

    // try to find key in object, if not found return default T
    template <class T>
    T get_value(string key, json::object object){
        if(object.contains(key)) {
            return json::value_to<T>(object.at(key));
        } else {
            return T();
        }
    }

    public:

    virtual void configure(
        json::object config,
        std::shared_ptr<mqtt::async_client> mqtt_client
    );

    // return our publication topic;
    string get_output_topic() { return output_topic; }

    // return our subscription topic;
    string get_input_topic() { return input_topic; }

    // Called with any message received on a subscribed topic
    void process_message(string topic, mqtt::const_message_ptr msg);

    // Flag to specify whether the processor is running or not 
    bool running = false;
    virtual void start(){ running = true;}
    virtual void stop(){ running = false;}
};
