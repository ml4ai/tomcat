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
using string = std::string;

class Agent;

// A base class for subscribed message handlers
class Processor : public Utils {

    // base location of testbed repo
    const std::string testbed_repo = 
        "https://gitlab.asist.aptima.com:5050/asist/testbed";

    // subscribed rollcall request
    const std::string rollcall_request_topic = 
        "agent/control/rollcall/request";
    const std::string rollcall_request_type = "agent";
    const std::string rollcall_request_sub_type = "rollcall:request";

    // subscribed trial start and stop
    const std::string trial_topic = "trial";
    const std::string trial_type = "trial";
    const std::string trial_sub_type_start = "start";
    const std::string trial_sub_type_stop = "stop";

    // published versioninfo (topic defined at config time)
    std::string version_info_topic = "version_topic_not_configured";
    const std::string version_info_type = "agent";
    const std::string version_info_sub_type = "versioninfo";

    // published heartbeat (topic defined at config time)
    std::string heartbeat_topic = "heartbeat_topic_not_configured";
    const std::string heartbeat_type = "status";
    const std::string heartbeat_sub_type = "heartbeat";

    // published rollcall response
    const std::string rollcall_response_topic =
        "agent/control/rollcall/response";
    const std::string rollcall_response_type = "agent";
    const std::string rollcall_response_sub_type = "rollcall:response";

    // start time of this class instantiation
    time_t start_time;

    // holds the result of the async heartbeat operation
    std::future<void> heartbeat_future;
    bool running = false; // publish regular heartbeats when true
    void publish_heartbeats();
    std::string status = "uninitialized";

    void update_heartbeat_message(json::object header, json::object msg);

  protected:
    // subscription
    json::array subscribes = json::array();

    // publication
    json::array publishes = json::array();

    // add an element to the publishes array
    void add_publication(
        const std::string topic,
        const std::string type,
        const std::string sub_type);

    // add an element to the subscribes array
    void add_subscription(
        const std::string topic,
        const std::string type,
        const std::string sub_type);

    // this software version
    std::string agent_version = "not_set";

    // this agent name on the Testbed
    std::string agent_name = "not_set";

    // owning institution
    std::string owner = "The University of Arizona";

    // Testbed git repository
    std::string testbed_source = "not_set";

    // agent using this class
    Agent* agent = nullptr;

    // Used for creating asynchronous heartbeat messages
    json::object heartbeat_header = json::object();
    json::object heartbeat_msg = json::object();

    json::object new_header(const json::object header,
                            const std::string timestamp,
                            const std::string type);

    json::object new_msg(const json::object msg,
                         const std::string timestamp,
                         const std::string sub_type);


  public:
    Processor();
    virtual void configure(const json::object& config, Agent *agent);
    virtual void process_message(const std::string topic,
                                 const json::object& message);
    void publish(const std::string topic, const json::object& message);
    void process_next_message();
    void publish_heartbeat_message();
    void start();
    void stop();
    std::vector<std::string> get_subscription_topics();
    std::vector<std::string> get_publication_topics();
};
