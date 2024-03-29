#pragma once

#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/json/array.hpp>
#include <boost/log/trivial.hpp>

#include "Utils.hpp"
#include <boost/json.hpp>
#include <future>
#include <queue>
#include <thread>

namespace json = boost::json;

// input / output message format:
// {
//   "topic": topic
//   "header": {
//     "version": testbed version or 0.1 if not in input
//     "message_type": TYPE
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
#define TESTBED_REPO "https://gitlab.asist.aptima.com:5050/asist/testbed"
#define OWNING_INSTITUTION "The University of Arizona"
#define AGENT_NAME "Processor"
#define AGENT_VERSION "1.0.0"

// subscriptions
#define TRIAL_TOPIC "trial"
#define TRIAL_TYPE "trial"
#define TRIAL_SUB_TYPE_START "start"
#define TRIAL_SUB_TYPE_STOP "stop"

#define ROLL_REQ_TOPIC "agent/control/rollcall/request"
#define ROLL_REQ_TYPE "agent"
#define ROLL_REQ_SUB_TYPE "rollcall:request"

// publications
#define HEARTBEAT_TOPIC "status/reference_agent/heartbeats"
#define HEARTBEAT_TYPE "status"
#define HEARTBEAT_SUB_TYPE "heartbeat"

#define ROLL_RES_TOPIC "agent/control/rollcall/response"
#define ROLL_RES_TYPE "agent"
#define ROLL_RES_SUB_TYPE "rollcall:response"

#define VERSION_TOPIC "agent/reference_agent/versioninfo"
#define VERSION_TYPE "agent"
#define VERSION_SUB_TYPE "versioninfo"

class Agent;

// A base class for subscribed message handlers
class Processor : public Utils {

    // start time of this class instantiation
    time_t start_time;

    // holds the result of the async heartbeat operation
    std::future<void> heartbeat_future;
    bool running = false; // publish regular heartbeats when true
    void publish_heartbeats();
    std::string status = "uninitialized";

  protected:
    // subscription
    json::array subscribes = json::array();
    void add_subscriptions(const json::object& config);

    // publication
    json::array publishes = json::array();
    void add_publications(const json::object& config);

    // Add an element to the publishes or subscribes array
    void add_bus_id(json::array &arr,
                    const std::string topic,
                    const std::string message_type,
                    const std::string sub_type);

    // this software version
    std::string agent_version = "not_set";

    // this agent name on the Testbed
    std::string agent_name = "not_set";

    // owning institution
    std::string owner = "not_set";

    // Testbed version
    std::string testbed_version = "1.0";

    // Testbed git repository
    std::string testbed_source = "not_set";

    // agent using this class
    Agent* agent = nullptr;

    // last received trial start or stop message, used for creating
    // asynchronous heartbeat messages
    json::object trial_message = json::object();

    void publish(const std::string output_topic,
                 const json::object& input_message,
                 const json::object& output_data);

    void publish(const std::string output_topic,
                 const std::string output_message_type,
                 const std::string output_sub_type,
                 const json::object& input_message,
                 const json::object& output_data);
    void publish_version_info_message(const json::object& input_message);
    void publish_rollcall_response_message(const json::object& input_message);

  public:
    Processor(Agent* agent);
    virtual void configure(const json::object& config);
    virtual void process_message(const json::object& message);
    void process_next_message();
    void publish_heartbeat_message();
    void start();
    void stop();
    std::vector<std::string> get_input_topics();
    std::vector<std::string> get_output_topics();
    std::vector<std::string> traffic_out, traffic_in;
};
