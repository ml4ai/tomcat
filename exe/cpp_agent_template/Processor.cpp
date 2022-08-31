#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include <boost/json/array.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <set>
#include <queue>
#include <thread>

#include "Agent.hpp"
#include "Processor.hpp"

namespace json = boost::json;

Processor::Processor(Agent* agent): agent(agent) {
    time(&start_time);

    // Subscriptions
    add_subscription(
        TRIAL_TOPIC,
	TRIAL_MESSAGE_TYPE,
	TRIAL_SUB_TYPE_STOP
    );
    add_subscription(
        TRIAL_TOPIC,
	TRIAL_MESSAGE_TYPE,
	TRIAL_SUB_TYPE_START
    );
    add_subscription(
        ROLLCALL_REQUEST_TOPIC,
	ROLLCALL_REQUEST_MESSAGE_TYPE,
	ROLLCALL_REQUEST_SUB_TYPE
    );

    // Publications
    add_publication(
        HEARTBEAT_TOPIC,
        HEARTBEAT_MESSAGE_TYPE,
        HEARTBEAT_SUB_TYPE
    );
    add_publication(
        ROLLCALL_RESPONSE_TOPIC,
        ROLLCALL_RESPONSE_MESSAGE_TYPE,
        ROLLCALL_RESPONSE_SUB_TYPE
    );
    add_publication(
        VERSION_INFO_TOPIC,
        VERSION_INFO_MESSAGE_TYPE,
        VERSION_INFO_SUB_TYPE
    );
}

// Set global variables 
void Processor::configure(const json::object &config) {
    agent_name = val<std::string>(config, "agent_name", AGENT_NAME);
    version = val<std::string>(config, "version", SOFTWARE_VERSION);
    owner = val<std::string>(config, "owner", OWNER);
    testbed_source = TESTBED 
        + std::string("/")
   	+ agent_name
       	+ std::string(":")
       	+ version;
}

// return a value with the three fields identifying a message
json::value Processor::create_bus_id(const std::string topic,
                                     const std::string message_type,
                                     const std::string sub_type) {
    json::value id = {
        { "topic", topic },
        { "message_type", message_type },
        { "sub_type", sub_type }
    };

    return id;
}

// Convenience method for creating a bus ID with only the topic specified
json::value Processor::create_bus_id(const std::string topic) {
    return create_bus_id(topic, "not_set", "not_set");
}

// add a bus ID to the subscribes array
void Processor::add_subscription(const std::string topic,
                                 const std::string message_type,
                                 const std::string sub_type) {
    subscribes.emplace_back(create_bus_id(topic, message_type, sub_type));
}

// convenience method to add a subscription with only the topic specified
void Processor::add_subscription(const std::string topic) {
    add_subscription(topic, "not_set", "not_set");
}

// add a bus ID to the publishes array
void Processor::add_publication(const std::string topic,
                                const std::string message_type,
                                const std::string sub_type) {
    publishes.emplace_back(create_bus_id(topic, message_type, sub_type));
}

// convenience method to add a publication with only the topic specified
void Processor::add_publication(const std::string topic) {
    add_publication(topic, "not_set", "not_set");
}

// Function that publishes heartbeat messages on an interval
void Processor::publish_heartbeats() {
    while (running) {
        std::this_thread::sleep_for(std::chrono::seconds(10));
	if(running) {
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
    heartbeat_future = std::async(
        std::launch::async,
        &Processor::publish_heartbeats,
        this
    );
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
    return unique_values(subscribes, "topic");
}

// Return a vector of the published topics
std::vector<std::string> Processor::get_output_topics() {
    return unique_values(publishes, "topic");
}

// Convenience method for publication without message_type or sub_type
void Processor::publish(
    const std::string output_topic,
    const json::object &input_message,
    const json::object &output_data) {

    publish(output_topic, "not_set", "not_set", input_message, output_data);
}

// Compose a complete message for publication
void Processor::publish(
	const std::string output_topic,
	const std::string output_message_type,
	const std::string output_sub_type,
        const json::object &input_message,
        const json::object &output_data) {

    std::string timestamp = get_timestamp();

    // Common header struct for output message
    json::object output_header;
    output_header["message_type"] = output_message_type;
    output_header["timestamp"] = timestamp;
    output_header["version"] = testbed_version;

    // Common msg struct for output message
    json::object output_msg;
    output_msg["message_type"] = output_sub_type;
    output_msg["source"] = agent_name;
    output_msg["version"] = version;
    output_msg["timestamp"] = timestamp;
    // copy these fields from the input msg struct if they exist
    // and are non-empty
    json::object input_msg = val<json::object>(input_message, "msg");
    std::string experiment_id = 
        val<std::string>(input_msg, "experiment_id");
    if(!experiment_id.empty()) {
        output_msg["experiment_id"] = experiment_id;
    }
    std::string trial_id = val<std::string>(input_msg, "trial_id");
    if(!trial_id.empty()) {
        output_msg["trial_id"] = trial_id;
    }
    std::string replay_id = val<std::string>(input_msg, "replay_id");
    if(!replay_id.empty()) {
        output_msg["replay_id"] = replay_id;
    }
    std::string replay_root_id = 
        val<std::string>(input_msg, "replay_root_id");
    if(!replay_root_id.empty()) {
        output_msg["replay_root_id"] = replay_root_id;
    }

    // assemble output message 
    json::object output_message;
    output_message["topic"] = output_topic;
    output_message["header"] = output_header;
    output_message["msg"] = output_msg;
    output_message["data"] = output_data;

    // publish output message
    agent->publish(output_message);

    // log outbound traffic
    traffic_out.push_back(output_topic);
}

// add subscriptions from the config struct.
void Processor::add_subscriptions(const json::object &config){
    json::array arr = val<json::array>(config, "subscribes");
    for(size_t i = 0 ;  i < arr.size() ; i++) {
        std::string topic = json::value_to<std::string>(arr.at(i));
        subscribes.emplace_back(create_bus_id(topic));
    }
}

// add publications from the config struct
void Processor::add_publications(const json::object &config){
    json::array arr = val<json::array>(config, "publishes");
    for(size_t i = 0 ;  i < arr.size() ; i++) {
        std::string topic = json::value_to<std::string>(arr.at(i));
        publishes.emplace_back(create_bus_id(topic));
    }
}

// process messages with Message Bus identifiers that match ours
void Processor::process_message(const json::object &input_message) {

    std::string topic = val<std::string>(input_message, "topic");
    traffic_in.push_back(topic);

    json::object header = val<json::object>(input_message,"header");
    std::string input_message_type = val<std::string>(header, "message_type");

    json::object msg = val<json::object>(input_message,"msg");
    std::string input_sub_type = val<std::string>(msg, "sub_type");

    // trial message
    if((topic.compare(TRIAL_TOPIC) == 0) &&
    (input_message_type.compare(TRIAL_MESSAGE_TYPE) == 0)) {

	// start
        if (input_sub_type.compare(TRIAL_SUB_TYPE_START) == 0) {

	    std::cout << "Trial started"  << std::endl;

	    // set the testbed version if the trial header has it
            json::object header = val<json::object>(input_message,"header");
	    std::string new_version = val<std::string>(header, "version");
	    if(!new_version.empty()) {
	        testbed_version = new_version;
	    }
	    trial_message = input_message;

	    publish_version_info_message(input_message);
	    if(running) {
	        publish_heartbeat_message();
	    }
        }
	// stop
	else if (input_sub_type.compare(TRIAL_SUB_TYPE_STOP) == 0) {
	    std::cout << "Trial stopped"  << std::endl;
            trial_message = input_message;
	    if(running) {
	        publish_heartbeat_message();
	    }
	    // Report the activity during the trial.
	    std::cout << "Messages read during Trial:" << std::endl;
	    count_keys(traffic_in);
	    std::cout << "Messages written during Trial:" << std::endl;
	    count_keys(traffic_out);
        }
    }

    // rollcall request message
    else if((topic.compare(ROLLCALL_REQUEST_TOPIC) == 0) &&
        (input_message_type.compare(ROLLCALL_REQUEST_MESSAGE_TYPE) == 0) &&
        (input_sub_type.compare(ROLLCALL_REQUEST_SUB_TYPE) == 0))
    {
        publish_rollcall_response_message(input_message);
    }

    agent->process_next_message();
}

// respond to Rollcall Request message
void Processor::publish_rollcall_response_message(
	const json::object &input_message) {

    // create rollcall response data
    time_t now;
    time(&now);
    int uptime = now-start_time;

    json::object input_data = val<json::object>(input_message,"data");
    json::object output_data;
    output_data["version"] = version;
    output_data["uptime"] = uptime;
    output_data["status"] = "up";
    output_data["rollcall_id"] = 
        val<std::string>(input_data, "rollcall_id", "not_set");

    publish(ROLLCALL_RESPONSE_TOPIC,
            ROLLCALL_RESPONSE_MESSAGE_TYPE,
            ROLLCALL_RESPONSE_SUB_TYPE,
            input_message,
            output_data);
}

// respond to Trial Start message
void Processor::publish_version_info_message(
    const json::object &input_message){

    // create version info data
    json::object output_data;
    output_data["agent_name"] = agent_name;
    output_data["owner"] = owner;
    output_data["version"] = version;
    output_data["source"] = testbed_source;
    output_data["publishes"] = publishes;
    output_data["subscribes"] = subscribes;

    publish(VERSION_INFO_TOPIC,
            VERSION_INFO_MESSAGE_TYPE,
            VERSION_INFO_SUB_TYPE,
            input_message,
            output_data);
}

// use the trial message as input
void Processor::publish_heartbeat_message() {

    // create heartbeat data
    json::object output_data;
    output_data["running"] = running;
    output_data["state"] = "ok";
    output_data["status"] = status;

    publish(HEARTBEAT_TOPIC,
            HEARTBEAT_MESSAGE_TYPE,
            HEARTBEAT_SUB_TYPE,
            trial_message,
            output_data);
}
