#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include <boost/json/array.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <set>
#include <thread>

#include "Agent.hpp"
#include "BaseMessageHandler.hpp"

using namespace std;
namespace json = boost::json;


BaseMessageHandler::BaseMessageHandler(Agent* agent): agent(agent) {
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

// return a value with the fields identifying a message on the message bus
json::value BaseMessageHandler::create_bus_id(const string topic,
                                              const string message_type,
                                              const string sub_type) {
    json::value id = {
        { "topic", topic },
        { "message_type", message_type },
        { "sub_type", sub_type }
    };

    return id;
}

void BaseMessageHandler::add_subscription(const string topic,
                                          const string message_type,
                                          const string sub_type) {
    subscribes.emplace_back(create_bus_id(topic, message_type, sub_type));
}

void BaseMessageHandler::add_subscription(const string topic) {
    add_subscription(topic, "not_set", "not_set");
}

void BaseMessageHandler::add_publication(const string topic,
                                         const string message_type,
                                         const string sub_type) {
    publishes.emplace_back(create_bus_id(topic, message_type, sub_type));
}

void BaseMessageHandler::add_publication(const string topic) {
    add_publication(topic, "not_set", "not_set");
}

// Using the config argument and hardcoded values, populate the global 
// Version Info data structure.   
void BaseMessageHandler::configure(const json::object &config) {
    status = "Configuring";

    // set up the version info data
    agent_name = val<string>(config, "agent_name", AGENT_NAME);
    version = val<string>(config, "version", SOFTWARE_VERSION);
    string owner = val<string>(config, "owner", OWNER);
    string testbed_source = 
        TESTBED + string("/") + agent_name + string(":") + version;
}

/** Function that publishes heartbeat messages while the agent is running */
void BaseMessageHandler::publish_heartbeats() {

    while (running) {
        this_thread::sleep_for(seconds(10));
	publish_heartbeat_message();
    }
}

// start publishing heartbeats
void BaseMessageHandler::start_heartbeats() {

    running = true;
    status = "I am processing messages";

    // send immediate ack
    publish_heartbeat_message();

    // Start threaded publishing
    heartbeat_future = async(
        launch::async,
        &BaseMessageHandler::publish_heartbeats,
        this
    );
}

// stop publishing heartbeats
void BaseMessageHandler::stop_heartbeats() {

    running = false;
    status = "stopped";

    // send immediate ack
    publish_heartbeat_message();

    heartbeat_future.wait();
}

vector<string> BaseMessageHandler::get_input_topics() {
    return unique_values(subscribes, "topic");
}

vector<string> BaseMessageHandler::get_output_topics() {
    return unique_values(publishes, "topic");
}

// create the common header struct for an outgoing message
json::object BaseMessageHandler::create_output_header(
        const json::object &input_message,
        const string timestamp,
	const string output_message_type) {

    json::object input_header = val<json::object>(input_message, "header");
    json::object output_header;
    output_header["message_type"] = output_message_type;
    output_header["timestamp"] = timestamp;
    output_header["version"] = testbed_version;

    return output_header;
}

// create the common msg struct for an outgoing message
json::object BaseMessageHandler::create_output_msg(
        const json::object &input_message,
        const string timestamp,
	const string output_sub_type) {

    json::object input_msg = val<json::object>(input_message, "msg");
    json::object output_msg;
    output_msg["message_type"] = output_sub_type;
    output_msg["source"] = agent_name;
    output_msg["version"] = version;
    output_msg["timestamp"] = timestamp;

    // add these only if they exist and are non-empty
    string experiment_id = val<string>(input_msg, "experiment_id");
    if(!experiment_id.empty()) {
        output_msg["experiment_id"] = experiment_id;
    }
    string trial_id = val<string>(input_msg, "trial_id");
    if(!trial_id.empty()) {
        output_msg["trial_id"] = trial_id;
    }
    string replay_id = val<string>(input_msg, "replay_id");
    if(!replay_id.empty()) {
        output_msg["replay_id"] = replay_id;
    }
    string replay_root_id = val<string>(input_msg, "replay_root_id");
    if(!replay_root_id.empty()) {
        output_msg["replay_root_id"] = replay_root_id;
    }

    return output_msg;
}


// process messages with Message Bus identifiers that match ours
void BaseMessageHandler::process_message(const string topic,
                                         const json::object &input_message) {

    string input_message_type = 
        val<string>(val<json::object>(input_message,"header"), "message_type");

    string input_sub_type = 
        val<string>(val<json::object>(input_message,"msg"), "sub_type");

    // process trial message
    if((topic.compare(TRIAL_TOPIC) == 0) &&
    (input_message_type.compare(TRIAL_MESSAGE_TYPE) == 0)) {

	// trial start
        if (input_sub_type.compare(TRIAL_SUB_TYPE_START) == 0) {

	    // set the testbed version if the trial header has it
            json::object header = val<json::object>(input_message,"header");
	    string new_version = val<string>(header, "version");
	    if(!new_version.empty()) {
	        testbed_version = new_version;
	    }

	    trial_message = input_message;
	    publish_version_info_message(input_message);
	    publish_heartbeat_message();
        }
	// trial stop
	else if (input_sub_type.compare(TRIAL_SUB_TYPE_STOP) == 0) {
            trial_message = input_message;
	    publish_heartbeat_message();
        }
    }

    // process rollcall request message
    else if((topic.compare(ROLLCALL_REQUEST_TOPIC) == 0) &&
        (input_message_type.compare(ROLLCALL_REQUEST_MESSAGE_TYPE) == 0) &&
        (input_sub_type.compare(ROLLCALL_REQUEST_SUB_TYPE) == 0))
    {
        publish_rollcall_response_message(input_message);
    }
}

void BaseMessageHandler::publish_rollcall_response_message(
	const json::object &input_message) {

    string timestamp = get_timestamp();

    // create common header for output message
    json::object output_header = create_output_header(
            input_message,
            timestamp,
            ROLLCALL_RESPONSE_MESSAGE_TYPE
    );

    // create common msg for output message
    json::object output_msg = create_output_msg(
            input_message,
	    timestamp,
	    ROLLCALL_RESPONSE_SUB_TYPE
    );

    // create data for output message
    time_t now;
    time(&now);
    int uptime = now-start_time;

    json::object input_data = val<json::object>(input_message,"data");
    json::object output_data;
    output_data["version"] = version;
    output_data["uptime"] = uptime;
    output_data["status"] = "up";
    output_data["rollcall_id"] = 
        val<string>(input_data, "rollcall_id", "not_set");

    // assemble finished message
    json::object output_message;
    output_message["header"] = output_header;
    output_message["msg"] = output_msg;
    output_message["data"] = output_data;

    // agent takes it from here
    agent->publish(ROLLCALL_RESPONSE_TOPIC, output_message);
}

void BaseMessageHandler::publish_version_info_message(
    const json::object &input_message){

    string timestamp = get_timestamp();

    // create common header
    json::object output_header = create_output_header(
        input_message,
        timestamp,
        VERSION_INFO_MESSAGE_TYPE
    );

    // create common msg
    json::object output_msg = create_output_msg(
        input_message,
        timestamp,
        VERSION_INFO_SUB_TYPE
    );

    // create data
    json::object output_data;
    output_data["publishes"] = publishes;
    output_data["subscribes"] = subscribes;

    // assemble outgoing message
    json::object output_message;
    output_message["header"] = output_header;
    output_message["msg"] = output_msg;
    output_message["data"] = output_data;

    // agent takes it from here
    agent->publish(VERSION_INFO_TOPIC, output_message);
}

// use the trial message as input
void BaseMessageHandler::publish_heartbeat_message() {

    string timestamp = get_timestamp();

    // create common header
    json::object output_header = create_output_header(
        trial_message,
        timestamp,
        HEARTBEAT_MESSAGE_TYPE
    );

    // create common msg
    json::object output_msg = create_output_msg(
        trial_message,
        timestamp,
        HEARTBEAT_SUB_TYPE
    );

    json::object output_data;
    output_data["running"] = running;
    output_data["state"] = state;
    output_data["status"] = status;

    // assemble outgoing message
    json::object output_message;
    output_message["header"] = output_header;
    output_message["msg"] = output_msg;
    output_message["data"] = output_data;

    // agent takes it from here
    agent->publish(HEARTBEAT_TOPIC, output_message);
}
