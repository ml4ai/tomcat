#pragma once

#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/log/trivial.hpp>
#include <boost/json/array.hpp>

#include <thread>
#include <queue>
#include <future>
#include <boost/json.hpp>
#include "Utils.hpp"


namespace json = boost::json;


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
    void publish_heartbeats();
    std::string status = "uninitialized";
		    
    // true if a message is currently being handled.  Input queued when true.
    bool processing = false;

    protected:

    // subscription
    json::array subscribes = json::array();
    std::vector<std::string> add_subscriptions(const json::object &config);
    void add_subscription(const std::string topic);
    void add_subscription(
        const std::string topic,
        const std::string message_type,
        const std::string sub_type
    );

    // publication
    json::array publishes = json::array();
    std::vector<std::string> add_publications(const json::object &config);
    void add_publication(const std::string topic);
    void add_publication(
        const std::string topic,
        const std::string message_type,
        const std::string sub_type
    );

    // configuration
    std::string version = "not_set";
    std::string agent_name = "not_set";
    std::string owner = "not_set";
    std::string testbed_version = "1.0";
    std::string testbed_source = "not_set";

    // File or MQTT operations
    Agent *agent = nullptr;

    // last received trial start or stop message
    json::object trial_message = json::object();

    json::value create_bus_id(
        const std::string topic,
        const std::string message_type,
        const std::string sub_type
    );

    void publish(
        const std::string output_topic,
        const json::object &input_message,
        const json::object &output_data
    );

    void publish(
        const std::string output_topic,
        const std::string output_message_type,
        const std::string output_sub_type,
        const json::object &input_message,
        const json::object &output_data
    );

    void publish_version_info_message(const json::object &input_message);
    void publish_rollcall_response_message(const json::object &input_message);

    public:

    BaseMessageHandler(Agent* agent);
    virtual void configure(const json::object &config);
    virtual void process_message(const json::object &message);
    void enqueue_message(const json::object &input_message);
    void process_next_message();
    void publish_heartbeat_message();
    void start();
    void stop();
    std::vector<std::string> get_input_topics();
    std::vector<std::string> get_output_topics();
    std::vector<std::string> traffic_out, traffic_in;
};