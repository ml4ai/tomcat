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

    vector<string> input_topics, output_topics;

    // this software version
    string version = "not_set";

    // config agent name
    string source = "not_set";

    // owner
    Agent *agent = nullptr;

    // version info message data
    json::object version_info_data;

    // return T value for key or default T if key not found
    template <class T>
    T val(const json::object &src, const string key) {
        if(src.contains(key)) {
            return json::value_to<T>(src.at(key));
        } else {
            return T(); 
        }
    }

    // return T value for key or fallback T if key not found
    template <class T>
    T val_or_else(const json::object &src,
                  const string key,
                  const T fallback) {

        if(src.contains(key)) {
            return json::value_to<T>(src.at(key));
        } else {
            return fallback;
        }
    }

    string get_timestamp();

    public:

    MessageHandler(Agent* agent, const json::object &config);
    MessageHandler(){}
    ~MessageHandler(){}

    virtual vector<string> get_input_topics() { return input_topics;}
    virtual vector<string> get_output_topics() { return output_topics;}

    void process_message(string topic, json::object message);
};
