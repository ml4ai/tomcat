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

class Agent;

// A base class for subscribed message handlers
class Processor : public Utils {

    // base location of testbed repo
    const std::string testbed_repo = 
        "https://gitlab.asist.aptima.com:5050/asist/testbed";

    // input rollcall request
    const std::string roll_req_topic = "agent/control/rollcall/request";
    const std::string roll_req_type = "agent";
    const std::string roll_req_sub_type = "rollcall:request";

    // input trial start and stop
    const std::string trial_topic = "trial";
    const std::string trial_type = "trial";
    const std::string trial_sub_type_start = "start";
    const std::string trial_sub_type_stop = "stop";

    // output versioninfo (topic defined at config time)
    std::string version_topic = "version_topic_not_configured";
    const std::string version_type = "agent";
    const std::string version_sub_type = "versioninfo";

    // output heartbeat (topic defined at config time)
    std::string heartbeat_topic = "heartbeat_topic_not_configured";
    const std::string heartbeat_type = "status";
    const std::string heartbeat_sub_type = "heartbeat";

    // output rollcall response
    const std::string roll_res_topic = "agent/control/rollcall/response";
    const std::string roll_res_type = "agent";
    const std::string roll_res_sub_type = "rollcall:response";

    // start time of this class instantiation
    time_t start_time;

    // holds the result of the async heartbeat operation
    std::future<void> heartbeat_future;
    bool running = false; // publish regular heartbeats when true
    void publish_heartbeats();
    std::string status = "uninitialized";

    // traffic reporting
    std::vector<std::string> traffic_out, traffic_in;

  protected:
    // subscription
    json::array subscribes = json::array();

    // publication
    json::array publishes = json::array();

    // add an element to the publishes array
    void add_publication(
        const std::string topic,
        const std::string message_type,
        const std::string sub_type);

    // add an element to the subscribes array
    void add_subscription(
        const std::string topic,
        const std::string message_type,
        const std::string sub_type);

    // this software version
    std::string agent_version = "not_set";

    // this agent name on the Testbed
    std::string agent_name = "not_set";

    // owning institution
    std::string owner = "The University of Arizona";

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
    Processor();
    virtual void configure(const json::object& config, Agent *agent);
    virtual void process_message(const json::object& message);
    void process_next_message();
    void publish_heartbeat_message();
    void start();
    void stop();
    std::vector<std::string> get_input_topics();
    std::vector<std::string> get_output_topics();
};
