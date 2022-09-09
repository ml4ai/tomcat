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
    version_info_topic = "agent/" + agent_name + "/versioninfo";

    // Create subscriptions
    add_subscription(trial_topic, trial_message_type, trial_sub_type_stop);
    add_subscription(trial_topic, trial_message_type, trial_sub_type_start);
    add_subscription(rollcall_request_topic, 
                     rollcall_request_message_type,
		     rollcall_request_sub_type);

    // Create Publications
    add_publication(heartbeat_topic, 
		    heartbeat_message_type,
		    heartbeat_sub_type);
    add_publication(rollcall_response_topic,
		    rollcall_response_message_type,
		    rollcall_response_sub_type);
    add_publication(version_info_topic,
		    version_info_message_type,
		    version_info_sub_type);

    // Create heartbeat message common header
    heartbeat_header = {
        { "message_type", heartbeat_message_type },
	{ "version", "0.1" }};

    // Create heartbeat message common msg
    heartbeat_msg = {
        { "sub_type", heartbeat_sub_type },
        { "source", agent_name },
	{ "version", agent_version }};
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

// Every topic from which a message may be processed
std::vector<std::string> Processor::get_subscription_topics() {
    return get_array_values(subscribes, "topic");
}

// Every topic to which a message may be published
std::vector<std::string> Processor::get_publication_topics() {
    return get_array_values(publishes, "topic");
}

// create publication common header struct
json::object Processor::new_header(const json::object header,
                                      const std::string timestamp, 
                                      const std::string message_type) {
    json::object new_header = {
        { "timestamp", timestamp },
        { "version", val<std::string>(header, "version", "1.0")},
	{ "message_type", message_type}};

    return new_header;
}

// create publication common msg struct 
json::object Processor::new_msg(const json::object msg, 
		                   const std::string timestamp, 
				   const std::string sub_type) {
    json::object new_msg = {
        { "source", agent_name },
        { "sub_type", sub_type },
        { "version", agent_version },
        { "timestamp", timestamp }};
    copy_if<std::string>(msg, new_msg, std::string("experiment_id"));
    copy_if<std::string>(msg, new_msg, std::string("trial_id"));
    copy_if<std::string>(msg, new_msg, std::string("replay_id"));
    copy_if<std::string>(msg, new_msg, std::string("replay_root_id"));

    return new_msg;
}


// process messages from subscribed topics
void Processor::process_message(const std::string topic,
                                const json::object& message) {

    std::string timestamp = get_timestamp();

    json::object header = val<json::object>(message, "header");
    json::object msg = val<json::object>(message, "msg");

    // filter message response by topic, message_type, and sub_type
    std::string message_type = val<std::string>(header, "message_type");
    std::string sub_type = val<std::string>(msg, "sub_type");

    // process trial message
    if ((trial_topic.compare(topic) == 0) &&
        (trial_message_type.compare(message_type) == 0)) {

        // trial start
        if (trial_sub_type_start.compare(sub_type) == 0) {
            std::cout << "Trial started" << std::endl;

	    // publish version info message
	    json::object version_info_message = {
	        { "header",
                    new_header(header,timestamp, version_info_message_type) },
                { "msg", new_msg(msg, timestamp, version_info_sub_type) },
                { "data", {
                    { "agent_name", agent_name },
                    { "owner", owner },
                    { "version", agent_version },
                    { "source", testbed_source },
                    { "publishes", publishes },
                    { "subscribes", subscribes }}}};
            publish(version_info_topic, version_info_message);
        }

        // trial stop
        else if (trial_sub_type_stop.compare(sub_type) == 0) {
            std::cout << "Trial stopped" << std::endl;
        } 
	
	// other sub types not subscribed
	else {
            return;
	}

	if(!running) {
	    return;
	}

        // set global heartbeat message components and publish
        heartbeat_header = 
            new_header(header,timestamp, heartbeat_message_type);
	heartbeat_msg = new_msg(msg, timestamp, heartbeat_sub_type);
        publish_heartbeat_message();
    }

    // process rollcall request message
    else if ((rollcall_request_topic.compare(topic) == 0) &&
             (rollcall_request_message_type.compare(message_type) == 0) &&
             (rollcall_request_sub_type.compare(sub_type) == 0)) {

	// find uptime
        time_t now;
        time(&now);
        int uptime = now - start_time;

	// create new data object    
        json::object data = val<json::object>(message, "data");
        json::object new_data = {
            { "version", agent_version },
            { "uptime", uptime },
            { "status", "up" }};
        copy_if<std::string>(data, new_data, std::string("rollcall_id"));

        // publish rollcall response message
        json::object rollcall_response_message = {
            { "header", 
                new_header(header,timestamp, rollcall_response_message_type) },
            { "msg", new_msg(msg, timestamp, rollcall_response_sub_type) },
            { "data", new_data }};
        publish(rollcall_response_topic, rollcall_response_message);
    }
}

// can be called asynchronously
void Processor::publish_heartbeat_message() {

    // refresh the timestamps
    std::string timestamp = get_timestamp();
    heartbeat_header["timestamp"] = timestamp;
    heartbeat_msg["timestamp"] = timestamp;

    json::object heartbeat_message = {
        { "header", heartbeat_header },
        { "msg", heartbeat_msg },
        { "data", {
            { "running", running },
            { "state", "ok" },
            { "status", status }}}};

    publish(heartbeat_topic, heartbeat_message);
}

void Processor::publish(const std::string topic, const json::object& message) {
    agent->publish(topic, message);
}

