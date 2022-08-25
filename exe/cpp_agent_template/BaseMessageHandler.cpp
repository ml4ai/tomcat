#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include <boost/json/array.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <set>
#include <queue>
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

    // start the asynchronous Message queue monitor
    queue_future = async(
        launch::async,
        &BaseMessageHandler::check_queue,
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
    queue_future.wait();
}

vector<string> BaseMessageHandler::get_input_topics() {
    return unique_values(subscribes, "topic");
}

vector<string> BaseMessageHandler::get_output_topics() {
    return unique_values(publishes, "topic");
}

void BaseMessageHandler::publish(
	const string output_topic,
	const string output_message_type,
	const string output_sub_type,
        const json::object &input_message,
        const json::object &output_data) {

    string timestamp = get_timestamp();

    // Common Header
    json::object output_header;
    json::object input_msg = val<json::object>(input_message, "msg");
    output_header["message_type"] = output_message_type;
    output_header["timestamp"] = timestamp;
    output_header["version"] = testbed_version;

    // Common Msg
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

    json::object output_message;
    output_message["topic"] = output_topic;
    output_message["header"] = output_header;
    output_message["msg"] = output_msg;
    output_message["data"] = output_data;

    published_topics.push_back(output_topic);
    agent->publish(output_message);
}

// Convenience method adding message_type and sub_type fields
void BaseMessageHandler::publish(
	const string output_topic,
        const json::object &input_message,
        const json::object &output_data) {

	publish(output_topic, "not_set", "not_set", input_message, output_data);
}

void BaseMessageHandler::enqueue_message(const json::object &input_message) {
    message_queue.push(input_message);
}

// check the message queue every second
void BaseMessageHandler::check_queue() {

    this_thread::sleep_for(seconds(1));

    while (running) {
        this_thread::sleep_for(seconds(1));
        if((message_queue.size() > 0) && !processing) {
            process_next_message();
        }
    }
}

void BaseMessageHandler::process_next_message(){
    if(message_queue.empty()) {
        processing = false;
    } else {
        processing = true;
	const json::object &obj = message_queue.front();
	const json::object &copy = json::object(obj);
        message_queue.pop();
	process_message(copy);
    }
}

// process messages with Message Bus identifiers that match ours
void BaseMessageHandler::process_message(const json::object &input_message) {

    string topic = val<string>(input_message, "topic");

    int size = message_queue.size();
    if(size > 3) {
        cout << "processing " << topic << " " << size << endl;
    } else {
        cout << "processing " << topic << " " << endl;
    }

    if(topic.empty()) {
        cerr << "BaseMessageHandler::process_message Error:" << endl;
        cerr << "No topic field in message, cannot process" << endl;
    }

    string input_message_type = 
        val<string>(val<json::object>(input_message,"header"), "message_type");

    string input_sub_type = 
        val<string>(val<json::object>(input_message,"msg"), "sub_type");

    // trial message
    if((topic.compare(TRIAL_TOPIC) == 0) &&
    (input_message_type.compare(TRIAL_MESSAGE_TYPE) == 0)) {

	// start
        if (input_sub_type.compare(TRIAL_SUB_TYPE_START) == 0) {

	    // set the testbed version if the trial header has it
            json::object header = val<json::object>(input_message,"header");
	    string new_version = val<string>(header, "version");
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
            trial_message = input_message;
	    if(running) {
	        publish_heartbeat_message();
	    }
        }
    }

    // rollcall request message
    else if((topic.compare(ROLLCALL_REQUEST_TOPIC) == 0) &&
        (input_message_type.compare(ROLLCALL_REQUEST_MESSAGE_TYPE) == 0) &&
        (input_sub_type.compare(ROLLCALL_REQUEST_SUB_TYPE) == 0))
    {
        publish_rollcall_response_message(input_message);
    }

    process_next_message();
}





// respond to Rollcall Request message
void BaseMessageHandler::publish_rollcall_response_message(
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
        val<string>(input_data, "rollcall_id", "not_set");

    publish(ROLLCALL_RESPONSE_TOPIC,
            ROLLCALL_RESPONSE_MESSAGE_TYPE,
            ROLLCALL_RESPONSE_SUB_TYPE,
            input_message,
            output_data);
}

// respond to Trial Start message
void BaseMessageHandler::publish_version_info_message(
    const json::object &input_message){

    // create version info data
    // TODO add other fields
    json::object output_data;
    output_data["publishes"] = publishes;
    output_data["subscribes"] = subscribes;

    publish(VERSION_INFO_TOPIC,
            VERSION_INFO_MESSAGE_TYPE,
            VERSION_INFO_SUB_TYPE,
            input_message,
            output_data);
}

// use the trial message as input
void BaseMessageHandler::publish_heartbeat_message() {

    // create heartbeat data
    json::object output_data;
    output_data["running"] = running;
    output_data["state"] = state;
    output_data["status"] = status;

    publish(HEARTBEAT_TOPIC,
            HEARTBEAT_MESSAGE_TYPE,
            HEARTBEAT_SUB_TYPE,
            trial_message,
            output_data);
}
