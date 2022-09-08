#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/json.hpp>
#include <boost/json/array.hpp>
#include <boost/log/trivial.hpp>
#include <queue>
#include <set>
#include <thread>

#include "Agent.hpp"
#include "Processor.hpp"

namespace json = boost::json;

Processor::Processor() {
    time(&start_time);
}

void Processor::configure(
    const json::object& config,
    Agent* agent) {

    this->agent = agent;

    // Set global variables from the configuration file object
    agent_name = val<std::string>(config, "agent_name", agent_name);
    agent_version = val<std::string>(config, "version", agent_version);
    owner = val<std::string>(config, "owner", owner);
    testbed_source = testbed_repo
        + std::string("/") + agent_name + std::string(":") + agent_version;

    // define these topics based on the agent name
    heartbeat_topic = "status/" + agent_name + "/heartbeats";
    version_topic = "agent/" + agent_name + "/versioninfo";

    // Create subscriptions
    add_subscription(trial_topic, trial_type, trial_sub_type_stop);
    add_subscription(trial_topic, trial_type, trial_sub_type_start);
    add_subscription(roll_req_topic, roll_req_type, roll_req_sub_type);

    // Create Publications
    add_publication(heartbeat_topic, heartbeat_type, heartbeat_sub_type);
    add_publication(roll_res_topic, roll_res_type, roll_res_sub_type);
    add_publication(version_topic, version_type, version_sub_type);
}

// Add an element to the publishes array
void Processor::add_publication(const std::string topic,
                                const std::string message_type,
                                const std::string sub_type) {
    publishes.emplace_back(json::value{{"topic", topic},
                                       {"message_type", message_type},
                                       {"sub_type", sub_type}});
}

// Add an element to the subscribes array
void Processor::add_subscription(const std::string topic,
                                 const std::string message_type,
                                 const std::string sub_type) {
    subscribes.emplace_back(json::value{{"topic", topic},
                                        {"message_type", message_type},
                                        {"sub_type", sub_type}});
}

// Function that publishes heartbeat messages on an interval
void Processor::publish_heartbeats() {
    while (running) {
        std::this_thread::sleep_for(std::chrono::seconds(10));
        if (running) {
            publish_heartbeat_message();
        }
    }
}

// start publishing heartbeats
void Processor::start() {

    running = true;
    status = "I am processing messages";

    // send immediate heartbeat to acknowledge startup
    publish_heartbeat_message();

    // Start the publishing thread
    heartbeat_future =
        std::async(std::launch::async, &Processor::publish_heartbeats, this);
}

// stop publishing heartbeats
void Processor::stop() {

    running = false;
    status = "Stopped";

    // send immediate heartbeat to acknowledge shutdown
    publish_heartbeat_message();

    // Stop the publishing thread
    heartbeat_future.wait();
}

// Return a vector of the subscribed topics
std::vector<std::string> Processor::get_input_topics() {
    return get_array_values(subscribes, "topic");
}

// Return a vector of the published topics
std::vector<std::string> Processor::get_output_topics() {
    return get_array_values(publishes, "topic");
}

// process messages with Message Bus identifiers that match ours
void Processor::process_message(const std::string input_topic,
                                const std::string input_type,      
                                const std::string input_sub_type,
                                const json::object& input_message) {
	
    traffic_in.push_back(input_topic);

    // trial message
    if ((trial_topic.compare(input_topic) == 0) &&
        (trial_type.compare(input_type) == 0)) {

        // trial start
        if (trial_sub_type_start.compare(input_sub_type) == 0) {
            std::cout << "Trial started" << std::endl;
	    // prepare to log the Message Bus traffic during the trial.
	    traffic_in.clear();
	    traffic_out.clear();
            traffic_in.push_back(input_topic);
            trial_message = input_message;
            publish_version_info_message(input_message);
            if (running) {
                publish_heartbeat_message();
            }
        }

        // trial stop
        else if (trial_sub_type_stop.compare(input_sub_type) == 0) {
            std::cout << "Trial stopped" << std::endl;
            trial_message = input_message;
            if (running) {
                publish_heartbeat_message();
            }
            // Report the processing traffic during the trial.
            std::cout << "Messages read during Trial: ";
	    std::cout << traffic_in.size() << std::endl;
            count_keys(traffic_in);
            std::cout << "Messages written during Trial: ";
	    std::cout << traffic_out.size() << std::endl;
            count_keys(traffic_out);
        }
    }

    // rollcall request message
    else if ((roll_req_topic.compare(input_topic) == 0) &&
             (roll_req_type.compare(input_type) == 0) &&
             (roll_req_sub_type.compare(input_sub_type) == 0)) {
        publish_rollcall_response_message(input_message);
    }

    agent->process_next_message();
}

// respond to Rollcall Request message
void Processor::publish_rollcall_response_message(
    const json::object& input_message) {

    // create rollcall response data
    time_t now;
    time(&now);
    int uptime = now - start_time;

    // get rollcall ID from input data
    json::object input_data = val<json::object>(input_message, "data");
    std::string rollcall_id = 
        val<std::string>(input_data, "rollcall_id", "not_set");

    json::object output_data = {
        { "version", agent_version },
        { "uptime", uptime },
        { "status", "up" },
        { "rollcall_id", rollcall_id }};

    publish(roll_res_topic,
            roll_res_type,
            roll_res_sub_type,
            input_message,
            output_data);
}

// respond to Trial Start message
void Processor::publish_version_info_message(
    const json::object& input_message) {

    // set the testbed_version global if the trial header has it
    json::object input_header = val<json::object>(input_message, "header");
    testbed_version = 
        val<std::string>(input_header, "version", testbed_version);

    // create version info data
    json::object output_data = {
        { "agent_name", agent_name },
        { "owner", owner },
        { "version", agent_version },
        { "source", testbed_source },
        { "publishes", publishes },
        { "subscribes", subscribes }};

    publish(version_topic,
            version_type,
            version_sub_type,
            input_message,
            output_data);
}

// use the trial message as input
void Processor::publish_heartbeat_message() {

    // create heartbeat data
    json::object output_data = {
        { "running", running },
        { "state", "ok" },
        { "status", status }};

    publish(heartbeat_topic,
            heartbeat_type, 
	    heartbeat_sub_type, 
	    trial_message, 
	    output_data);
}

// Compose a complete message for publication
void Processor::publish(const std::string output_topic,
                        const std::string output_message_type,
                        const std::string output_sub_type,
                        const json::object& input_message,
                        const json::object& output_data) {

    std::string timestamp = get_timestamp();

    // Common msg struct fields
    json::object output_msg = {
        { "message_type", output_sub_type },
        { "source", agent_name },
        { "version", agent_version },
        { "timestamp", timestamp }};

    // Common msg struct fields that may or may not exist
    json::object input_msg = val<json::object>(input_message, "msg");
    add_if<std::string>(input_msg, output_msg, std::string("experiment_id"));
    add_if<std::string>(input_msg, output_msg, std::string("trial_id"));
    add_if<std::string>(input_msg, output_msg, std::string("replay_id"));
    add_if<std::string>(input_msg, output_msg, std::string("replay_root_id"));

    // compose output message
    json::object output_message = {
        { "topic", output_topic },
	{ "header", {
	    { "message_type", output_message_type },
	    { "timestamp", timestamp },
	    { "version", testbed_version }}},
	{ "msg", output_msg },
	{ "data", output_data }};

    // publish output message
    agent->publish(output_message);

    // log outbound traffic
    traffic_out.push_back(output_topic);
}
