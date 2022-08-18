#pragma once

#include <string>
#include <boost/json.hpp>
#include <mqtt/async_client.h>

namespace json = boost::json;
using namespace std;

// input / output message format:
// {
//   "topic": topic
//   "header": {
//     "version": testbed version or 0.1 if not in input
//     "message_type": message_type
//     "timestamp": UTC timestamp in ISO-8601 format
//   },
//   "msg": {
//     "trial_id": field not included if empty in input
//     "timestamp": UTC timestamp in ISO-8601 format
//     "replay_id": field not included if empty in input
//     "version": agent version
//     "replay_root_id": field not included empty in input
//     "sub_type": sub_type
//     "source": agent name
//     "experiment_id": field not included empty in input
//   },
//   "data": {
//     // anything goes 
//   }
// }


class Agent;

// A base class for subscribed message handlers
class MessageHandler {

    private:

    json::stream_parser json_parser;

    protected:
    // subscription configuration
    string input_topic;
    string input_message_type;
    string input_sub_type;

    // publication configuration
    string output_topic;
    string output_message_type;
    string output_sub_type;
    string version;
    string source;

    // Flag to specify whether the processor is running or not 
    bool running = false;

    // MQTT client for Message Bus comms
    std::shared_ptr<mqtt::async_client> mqtt_client;

    // copy src string value if nonempty else delete dst key
    json::object update_nonempty_string(
        json::object src,
       	json::object dst,
       	string key
    );

    // Return the input configuration config name, e.g. "trial_start"
    virtual string get_input_config_name(){ return "";}

    // return the output configuration config name, e.g. "heartbeat"
    virtual string get_output_config_name(){ return "";}

    // return any value for key
    template <class T>
    T val(string key, json::object object){
        if(object.contains(key)) {
            return json::value_to<T>(object.at(key));
        } else {
            return T(); // return a default value if not found
        }
    }

    // return output data element for extending class
    virtual json::object get_data(json::object input_data) {
        return json::object();
    }

    void publish(json::value jv);
    string get_timestamp();
    virtual bool valid_input_header(json::object input_header);
    virtual bool valid_input_msg(json::object input_msg);
    virtual void process_message(json::object input_message);
    void process_header(
        json::object input_message,
        json::object output_message,
	string timestamp
    );
    void process_msg(
        json::object input_message,
        json::object output_message,
	string timestamp
    );
    void process_data(
        json::object input_message,
        json::object output_message
    );

    public:

    void process_message(string topic, mqtt::const_message_ptr msg);

    virtual void configure(
        json::object config,
        std::shared_ptr<mqtt::async_client> mqtt_client
    );

    // disable agent publishing
    virtual void start() { running = true;}

    // enable agent publishing
    virtual void stop() { running = false;}

    // return the running flag
    bool is_running() { return running; }

    // return our publication topic;
    string get_output_topic() { return output_topic; }

    // return our subscription topic;
    string get_input_topic() { return input_topic; }
};
