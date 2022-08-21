#pragma once

#include <string>
#include <boost/json.hpp>

namespace json = boost::json;
using namespace std;

// input / output message format:
// {
//   "header": {
//     "version": testbed version or 0.1 if not in input
//     "message_type": MESSAGE_TYPE
//     "timestamp": UTC timestamp in ISO-8601 format
//   },
//   "msg": {
//     "trial_id": field not included if empty in input
//     "timestamp": UTC timestamp in ISO-8601 format
//     "replay_id": field not included if empty in input
//     "version": agent version
//     "replay_root_id": field not included empty in input
//     "sub_type": SUB_TYPE
//     "source": agent name
//     "experiment_id": field not included empty in input
//   },
//   "data": {
//       json::object // anything goes
//   }
// }

#define TESTBED "https://gitlab.asist.aptima.com:5050/asist/testbed"

// subscriptions
#define TRIAL_TOPIC "trial"
#define TRIAL_MESSAGE_TYPE "trial"
#define TRIAL_SUB_TYPE_START "start"
#define TRIAL_SUB_TYPE_STOP "stop"

#define ROLLCALL_REQUEST_TOPIC "agent/control/rollcall/request"
#define ROLLCALL_REQUEST_MESSAGE_TYPE "agent"
#define ROLLCALL_REQUEST_SUB_TYPE "rollcall:request"

// publications
#define HEARTBEAT_TOPIC "status/reference_agent/heartbeats"
#define HEARTBEAT_MESSAGE_TYPE "status"
#define HEARTBEAT_SUB_TYPE "heartbeat"

#define ROLLCALL_RESPONSE_TOPIC "agent/control/rollcall/response"
#define ROLLCALL_RESPONSE_MESSAGE_TYPE "agent"
#define ROLLCALL_RESPONSE_SUB_TYPE "rollcall:response"

#define VERSION_INFO_TOPIC "agent/reference_agent/versioninfo"
#define VERSION_INFO_MESSAGE_TYPE "agent"
#define VERSION_INFO_SUB_TYPE "versioninfo"

class Agent;

// A base class for subscribed message handlers
class BaseMessageHandler {

    protected:

    // values read from config file
    string version = "not_set";
    string agent_name = "not_set";
    string owner = "not_set";

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

    void append_array(const json::object &src,
                      json::object &dst,
                      const string key);

    public:

    BaseMessageHandler(Agent* agent, const json::object &config);
    BaseMessageHandler(){}
    ~BaseMessageHandler(){}

    virtual vector<string> get_input_topics();
    virtual vector<string> get_output_topics();

    virtual void process_message(const string topic, const json::object &message);
};
