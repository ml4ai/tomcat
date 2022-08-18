#pragma once

#include <string>
#include <boost/json.hpp>

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
//       json::object // anything goes
//   }
// }


class Agent;

// A base class for subscribed message handlers
class MessageHandler {


    protected:


    // owner
    Agent *agent = nullptr;
   
    // Message Bus configuration
    json::object config;

    // last message received
    json::object prev_message;

    // Flag to specify whether the processor is running or not 
    bool running = false;

    // copy src string value if nonempty else delete dst key
    json::object update_nonempty_string(
        json::object src,
       	json::object dst,
       	string key
    );

    vector<string> subscriptions, publications;

    // return any value for key
    template <class T>
    T val(json::object object, string key){
        if(object.contains(key)) {
            return json::value_to<T>(object.at(key));
        } else {
            return T(); // return a default value if not found
        }
    }

    void publish(json::object output_message);

    void write(json::value jv);
    string get_timestamp();
    bool valid_input_header(json::object input_header);
    bool valid_input_msg(json::object input_msg);
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

    bool is_subscribed(string topic);

    public:

    virtual string get_output_message_type(){ return "not set"; }
    virtual string get_output_sub_type(){ return "not set"; }

    void process_message(string topic, json::object message);

    void configure(json::object config, Agent *agent);

    // Return the input topics
    vector<string> get_subscriptions(){return subscriptions;}

    // Return the output topics
    vector<string> get_publications(){return publications;}

    // enable agent
    void start() { running = true;}

    // disable agent
    void stop() { running = false;}

    // return the running flag
    bool is_running() { return running; }
};
