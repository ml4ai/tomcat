#pragma once

#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/log/trivial.hpp>

#include <string>
#include <thread>
#include <future>
#include <boost/json.hpp>


namespace json = boost::json;
using namespace std;
using namespace std::chrono;


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

    // holds the result of the async heartbeat operation
    std::future<void> heartbeat_future;

    private:

    bool running = false; // publish regular heartbeats when true
    string state = "ok";
    string status = "Uninitialized";
    string testbed_version = "1.0";

    void publish_heartbeat();
    void publish_version_info(json::object input_message);
    void publish_rollcall_response(json::object input_message);
    void publish_heartbeats();

    protected:

    time_t start_time;

    // values read from config file
    string version = "not_set";
    string agent_name = "not_set";

    // owner
    Agent *agent = nullptr;

    // last received trial start or stop message
    json::object trial_message = json::object();

    // version info message data
    json::object version_info_data = json::object();

    // return T value for key or default T if key not found or value is null
    template <class T>
    T val(const json::object &src, const string key) {
        if(src.contains(key)) {
	    if(src.at(key) == nullptr) {
                return T(); 
	    }
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

    json::object create_output_header(const json::object &input_header,
		                      const string timestamp,
                                      const string output_message_type);

    json::object create_output_msg(const json::object &input_msg,
                                   const string timestamp,
                                   const string output_sub_type);

    string get_message_type(const json::object &message);
    string get_sub_type(const json::object &message);

    vector<string> get_array_field(const string name, const string field);

    public:

    BaseMessageHandler(Agent* agent);
    virtual void configure(const json::object &config);

    vector<string> get_input_topics();
    vector<string> get_output_topics();

    virtual void process_message(const string topic,
		                 const json::object &message);

    void start_heartbeats();
    void stop_heartbeats();
};
