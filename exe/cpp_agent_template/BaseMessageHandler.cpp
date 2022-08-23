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
}

// Using the config argument and hardcoded values, populate the global 
// Version Info data structure.   
void BaseMessageHandler::configure(const json::object &config) {

    agent_name = val_or_else<string>(config, "agent_name", "not_set");
    version = val_or_else<string>(config, "version", "not_set");

    status = "Configuring";

    // set up the version info data
    string owner = val_or_else<string>(config, "owner", "not_set");
    string testbed_source = 
        TESTBED + string("/") + agent_name + string(":") + version;

    how about we just keep the config around and make this data 
	    when we need it?

	    Mutable globals suck

    json::value jv = {
        { "agent_name", agent_name },
	{ "version", version },
	{ "owner", owner },
	{ "source", { testbed_source } },
	{ "subscribes", {
            {
                { "topic", TRIAL_TOPIC },
                { "message_type", TRIAL_MESSAGE_TYPE },
                { "sub_type", TRIAL_SUB_TYPE_START }
            }, 
            {
                { "topic", TRIAL_TOPIC },
                { "message_type", TRIAL_MESSAGE_TYPE },
                { "sub_type", TRIAL_SUB_TYPE_STOP }
            }, 
            {
                { "topic", ROLLCALL_REQUEST_TOPIC },
                { "message_type", ROLLCALL_REQUEST_MESSAGE_TYPE },
                { "sub_type", ROLLCALL_REQUEST_SUB_TYPE }
            }
        }},
	{ "publishes" , {
            {
                { "topic", HEARTBEAT_TOPIC },
                { "message_type", HEARTBEAT_MESSAGE_TYPE },
                { "sub_type", HEARTBEAT_SUB_TYPE }
            }, 
            {
                { "topic", ROLLCALL_RESPONSE_TOPIC },
                { "message_type", ROLLCALL_RESPONSE_MESSAGE_TYPE },
                { "sub_type", ROLLCALL_RESPONSE_SUB_TYPE }
            }, 
            {
                { "topic", VERSION_INFO_TOPIC },
                { "message_type", VERSION_INFO_MESSAGE_TYPE },
                { "sub_type", VERSION_INFO_SUB_TYPE }
            }
	}}
    };

    version_info_data = json::value_to<json::object>(jv);

    // add config subscriptions and publications
    append_array(config, version_info_data, "publishes");
    append_array(config, version_info_data, "subscribes");
}

/** Function that publishes heartbeat messages while the agent is running */
void BaseMessageHandler::publish_heartbeats() {

    while (running) {
        this_thread::sleep_for(seconds(10));
        publish_heartbeat();
    }
}

void BaseMessageHandler::publish_heartbeat() {
    string timestamp = get_timestamp();

    cout << "BaseMessageHandler::publish_heartbeat()" << endl;
    /*
    // create common header
    json::object output_header = create_output_header(
        input_message,
        timestamp,
        HEARTBEAT_MESSAGE_TYPE
    );

    // create common msg
    json::object output_msg = create_output_msg(
        input_message,
        timestamp,
        HEARTBEAT_SUB_TYPE
    );

    json::value jv_data = {
        { "running", running },
	{ "state", state },
	{ "status", status }
    };

    // assemble outgoing message
    json::object output_message;
    output_message["header"] = output_header;
    output_message["msg"] = output_msg;
    output_message["data"] = heartbeat_data;

    // agent takes it from here
    agent->publish(HEARTBEAT_TOPIC, output_message);
    */
}

// start publishing heartbeats
void BaseMessageHandler::start_heartbeats() {

    running = true;
    status = "I am processing messages";

    // send immediate ack
    publish_heartbeat();

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
    publish_heartbeat();

    heartbeat_future.wait();
}

// append the dst array to the src array at key
void BaseMessageHandler::append_array(const json::object &src,
                                      json::object &dst,
                                      const string key){

    json::array src_array = val<json::array>(src, key);
    json::array dst_array = val<json::array>(dst, key);
    for(size_t i = 0 ;  i < src_array.size() ; i++) {
	dst_array.emplace_back(src_array.at(i));
    }
    dst[key] = dst_array;
}

vector<string> BaseMessageHandler::get_array_field(const string name,
                                                   const string field) {

    set<string> values;

    json::array arr = val<json::array>(version_info_data, name);
    for(size_t i = 0 ;  i < arr.size() ; i++) {
        json::value element = arr.at(i);
        string value = json::value_to<std::string>(element.at(field));
	if(!value.empty()) {
            values.insert(value);
	}
    }

    vector<string> ret;
    for(auto &value : values) {
        ret.push_back(value);
    }

    return ret;
}

vector<string> BaseMessageHandler::get_input_topics() {
    return get_array_field("subscribes", "topic");
}

vector<string> BaseMessageHandler::get_output_topics() {
    return get_array_field("publishes", "topic");
}

/** Get current UTC timestamp in ISO-8601 format. */
string BaseMessageHandler::get_timestamp() {
    return boost::posix_time::to_iso_extended_string(
        boost::posix_time::microsec_clock::universal_time()
    ) + "Z";
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

    json::value jv = {
        { "message_type", output_message_type },
        { "timestamp", timestamp },
        { "version", testbed_version }
    }


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

// return the message["header"]["message_type"] value
string BaseMessageHandler::get_message_type(const json::object &message) {
    return val<string>(val<json::object>(message,"header"), "message_type");
}

// return the message["msg"]["sub_type"] value
string BaseMessageHandler::get_sub_type(const json::object &message) {
    return val<string>(val<json::object>(message,"msg"), "sub_type");
}

void BaseMessageHandler::publish_version_info(json::object input_message){

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

    // TODO create data
    json::object output_data = json::object();

    // assemble outgoing message
    json::object output_message;
    output_message["header"] = output_header;
    output_message["msg"] = output_msg;
    output_message["data"] = output_data;

    // agent takes it from here
    agent->publish(VERSION_INFO_TOPIC, output_message);
}


// process messages that match our input fields
void BaseMessageHandler::process_message(const string topic,
                                         const json::object &input_message) {

    string input_message_type = get_message_type(input_message);
    string input_sub_type = get_sub_type(input_message);

    // If this is a trial message, branch on start or stop
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
	    publish_version_info(input_message);
	    publish_heartbeat();
        }
	// trial stop
	else if (input_sub_type.compare(TRIAL_SUB_TYPE_STOP) == 0) {
            trial_message = input_message;
            publish_heartbeat();
        }
    }

    // If this is a rollcall request, reply with a rollcall response message
    else if((topic.compare(ROLLCALL_REQUEST_TOPIC) == 0) &&
        (input_message_type.compare(ROLLCALL_REQUEST_MESSAGE_TYPE) == 0) &&
        (input_sub_type.compare(ROLLCALL_REQUEST_SUB_TYPE) == 0))
    {
        publish_rollcall_response(input_message);
    }
}

void BaseMessageHandler::publish_rollcall_response(
	json::object input_message) {

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
        val_or_else<string>(input_data, "rollcall_id", "not set");

    // assemble finished message
    json::object output_message;
    output_message["header"] = output_header;
    output_message["msg"] = output_msg;
    output_message["data"] = output_data;

    // agent takes it from here
    agent->publish(ROLLCALL_RESPONSE_TOPIC, output_message);
}
