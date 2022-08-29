#pragma once

#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/log/trivial.hpp>
#include <boost/json/array.hpp>

#include <string>
#include <thread>
#include <queue>
#include <future>
#include <boost/json.hpp>
#include "Utils.hpp"


namespace json = boost::json;
using namespace std;
using namespace std::chrono;


// input / output message format:
// {
//   "topic": topic
//   "header": {
//     "version": testbed version or 0.1 if not in input
//     "message_type": MESSAGE_TYPE
//     "timestamp": UTC timestamp in ISO-8601 format
//   },
//   "msg": {
//     "trial_id": from input, not included if empty
//     "timestamp": UTC timestamp in ISO-8601 format
//     "replay_id": from input, not included if empty
//     "version": agent version
//     "replay_root_id": from input, not included if empty
//     "sub_type": SUB_TYPE
//     "source": agent name
//     "experiment_id": from input, not included if empty
//   },
//   "data": {
//       json::object // anything goes
//   }
// }

// defaults for VersionInfoMessage if no config file is used.
#define TESTBED "https://gitlab.asist.aptima.com:5050/asist/testbed"
#define AGENT_NAME "BaseMessageHandler"
#define OWNER "The University of Arizona"
#define SOFTWARE_VERSION "1.0.0"

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
class BaseMessageHandler : public Utils {

    // the start time is recorded for uptime computation.
    time_t start_time;

    // holds the result of the async heartbeat operation
    std::future<void> heartbeat_future;
    bool running = false; // publish regular heartbeats when true
    string testbed_version = "1.0";
    void publish_heartbeats();
    void publish_heartbeat_message();
    string status = "uninitialized";
		    
    // true if a message is currently being handled.  Queue input
    // that happens when true.
    bool processing = false;

    json::array subscribes = json::array();
    json::array publishes = json::array();

    protected:

    vector<string> add_subscriptions(const json::object &config);
    vector<string> add_publications(const json::object &config);

    void add_subscription(const string topic);
    void add_subscription(
        const string topic,
        const string message_type,
        const string sub_type
    );

    void add_publication(const string topic);
    void add_publication(
        const string topic,
        const string message_type,
        const string sub_type
    );

    // values read from config file
    string version = "not_set";
    string agent_name = "not_set";

    // owner
    Agent *agent = nullptr;

    // last received trial start or stop message
    json::object trial_message = json::object();

    // version info message data
    json::object version_info_data = json::object();

    json::value create_bus_id(
        const string topic,
        const string message_type,
        const string sub_type
    );

    void publish(
        const string output_topic,
        const json::object &input_message,
        const json::object &output_data
    );

    void publish(
        const string output_topic,
        const string output_message_type,
        const string output_sub_type,
        const json::object &input_message,
        const json::object &output_data
    );

    void publish_version_info_message(const json::object &input_message);
    void publish_rollcall_response_message(const json::object &input_message);

    public:

    BaseMessageHandler(Agent* agent);
    virtual void configure(const json::object &config);

    void enqueue_message(const json::object &input_message);
    void process_next_message();

    vector<string> get_input_topics();
    vector<string> get_output_topics();
    vector<string> traffic_out, traffic_in;

    virtual void process_message(const json::object &message);

    void start_heartbeats();
    void stop_heartbeats();
};
