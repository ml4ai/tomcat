#pragma once

#include <string>
#include <boost/json.hpp>
#include <mqtt/async_client.h>

namespace json = boost::json;
using namespace std;

class Coordinator;

// A base class for subscribed message handlers
class MessageHandler {

    private:

    json::stream_parser json_parser;

    protected:
    // subscription
    string input_topic;
    string input_message_type;
    string input_sub_type;

    // publication
    string output_topic;
    json::object header; // Message Bus Common Header 
    json::object msg; // Message Bus Common Msg

    // publication to the Message Bus
    std::shared_ptr<mqtt::async_client> mqtt_client;

    // configuration data for all agents
    json::object config;

    // publish value on output_topic
    void publish(json::value jv);

    // timestamps
    string get_timestamp();

    // return the message for publication
    virtual json::object get_message(
        json::object input_header,
        json::object input_msg,
        json::object input_data
    );

    // return the header object for publication
    virtual json::object get_header(
	json::object output_header,
       	json::object input_header,
        string timestamp
    );

    // return the msg object for publication
    virtual json::object get_msg(
	json::object output_msg,
       	json::object input_msg,
        string timestamp
    );

    // return the data object for publication
    virtual json::object get_data(json::object input_data) {
        return json::object();
    }

    // copy key value from src to dst or erase key from dst if not found
    void copy_or_erase(
        json::object dst,
        json::object src,
        string key
    );

    // Return the input configuration config name, e.g. "trial_start"
    virtual string get_input_config_name(){ return "";}

    // return the output configuration config name, e.g. "heartbeat"
    virtual string get_output_config_name(){ return "";}

    // called with the successfully JSON parsed MQTT message payload
    virtual void process_json_message(json::object json_message);

    // try to find class T value for key in object
    template <class T>
    T get_value(string key, json::object object){
        if(object.contains(key)) {
            return json::value_to<T>(object.at(key));
        } else {
            return T(); // return a default value if not found
        }
    }

    public:

    // set global variables based on config input
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
