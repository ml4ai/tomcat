#pragma once

#include <string>
#include <boost/json.hpp>
#include <mqtt/async_client.h>

namespace json = boost::json;
using namespace std;

// input / output message format:
// {
//   "topic": "trial",
//   "header": {
//     "version": testbed version or 0.1 if not in input,
//     "message_type": "trial",
//     "timestamp": "2022-03-24T18:33:18.8040Z"
//   },
//   "msg": {
//     "trial_id": field not included if not in input,
//     "timestamp": "2022-03-24T18:33:18.8040Z",
//     "replay_parent_type": field not included if not in input,
//     "replay_id": field not included if not in input,
//     "version": agent version
//     "replay_parent_id": field not included if not in input,
//     "sub_type": "start",
//     "source": agent name
//     "experiment_id": field not included if not in input
//   },
//   "data": {
//     // anything goes 
//   }
// }


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

    // configuration data for all agents
    json::object config;

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
