#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include <boost/json/array.hpp>
#include <mqtt/async_client.h>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <set>

#include "Agent.hpp"
#include "BaseMessageHandler.hpp"

using namespace std;
namespace json = boost::json;


BaseMessageHandler::BaseMessageHandler(Agent* agent): agent(agent) {
    time(&start_time);
}

void BaseMessageHandler::configure(const json::object &config) {

    agent_name = val_or_else<string>(config, "agent_name", "not_set");
    version = val_or_else<string>(config, "version", "not_set");

    // set up the version info data
    string owner = val_or_else<string>(config, "owner", "not_set");
    string testbed_source = 
        TESTBED + string("/") + agent_name + string(":") + version;

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
                { "topic", ROLLCALL_RESPONSE_TOPIC },
                { "message_type", ROLLCALL_RESPONSE_MESSAGE_TYPE },
                { "sub_type", ROLLCALL_RESPONSE_SUB_TYPE }
            }, 
            {
                { "topic", VERSION_INFO_TOPIC },
                { "message_type", VERSION_INFO_MESSAGE_TYPE },
                { "sub_type", VERSION_INFO_SUB_TYPE }
            }, 
            {
                { "topic", HEARTBEAT_TOPIC },
                { "message_type", HEARTBEAT_MESSAGE_TYPE },
                { "sub_type", HEARTBEAT_SUB_TYPE }
            }
	}}
    };

    version_info_data = json::value_to<json::object>(jv);

    // add config subscriptions and publications
    append_array(config, version_info_data, "publishes");
    append_array(config, version_info_data, "subscribes");
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

// return the topics from "subscribes" or "publishes" fields
vector<string> BaseMessageHandler::get_topics(const string which) {

    set<string> topics;

    json::array arr = val<json::array>(version_info_data, which);
    for(size_t i = 0 ;  i < arr.size() ; i++) {
        json::value element = arr.at(i);
        string topic = json::value_to<std::string>(element.at("topic"));
	if(!topic.empty()) {
            topics.insert(topic);
	}
    }

    vector<string> ret;
    for(auto& topic : topics) {
        ret.push_back(topic);
    }

    return ret;
}


vector<string> BaseMessageHandler::get_input_topics() {
    return get_topics("subscribes");
}


vector<string> BaseMessageHandler::get_output_topics() {
    return get_topics("publishes");
}

/** Get current UTC timestamp in ISO-8601 format. */
string BaseMessageHandler::get_timestamp() {
    return boost::posix_time::to_iso_extended_string(
        boost::posix_time::microsec_clock::universal_time()
    ) + "Z";
}

// create common header struct for an outgoing message
json::object BaseMessageHandler::create_output_header(
        const json::object &input_message,
        const string timestamp,
	const string output_message_type) {

    json::object input_header = val<json::object>(input_message, "header");
    json::object output_header;
    output_header["message_type"] = output_message_type;
    output_header["timestamp"] = timestamp;
    output_header["version"] = 
        val_or_else<string>(input_header, "version", "1.0");

    return output_header;
}

// create common msg struct for an outgoing message
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

    // add these if they exist and are non-empty
    if(!val<string>(input_msg, "experiment_id").empty()) {
        output_msg["experiment_id"] = input_msg.at("experiment_id");
    }
    if(!val<string>(input_msg, "trial_id").empty()) {
        output_msg["trial_id"] = input_msg.at("trial_id");
    }
    if(!val<string>(input_msg, "replay_id").empty()) {
        output_msg["replay_id"] = input_msg.at("replay_id");
    }
    if(!val<string>(input_msg, "replay_root_id").empty()) {
        output_msg["replay_root_id"] = input_msg.at("replay_root_id");
    }

    return output_msg;
}

// return the message["header"]["message_type"] value
string BaseMessageHandler::get_message_type(const json::object &message) {
    json::object header = val<json::object>(message,"header");    
    return val<string>(header, "message_type");
}

// return the message["msg"]["sub_type"] value
string BaseMessageHandler::get_sub_type(const json::object &message) {
    json::object msg = val<json::object>(message,"msg");    
    return val<string>(msg, "sub_type");
}

// process messages that match our input fields
void BaseMessageHandler::process_message(const string topic,
                                         const json::object &input_message) {

    string input_message_type = get_message_type(input_message);
    string input_sub_type = get_sub_type(input_message);

    string timestamp = get_timestamp();


    // If this is a trial start, reply with a version info message
    if((topic.compare(TRIAL_TOPIC) == 0) &&
        (input_message_type.compare(TRIAL_MESSAGE_TYPE) == 0) &&
        (input_sub_type.compare(TRIAL_SUB_TYPE_START) == 0))
    {
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

	// data is already defined
	json::object output_data = version_info_data;

	// assemble outgoing message
	json::object output_message;
	output_message["header"] = output_header;
	output_message["msg"] = output_msg;
	output_message["data"] = output_data;

	// agent takes it from here
	agent->write(VERSION_INFO_TOPIC, output_message);
    }

    // If this is a rollcall request, reply with a rollcall response message
    else if((topic.compare(ROLLCALL_REQUEST_TOPIC) == 0) &&
        (input_message_type.compare(ROLLCALL_REQUEST_MESSAGE_TYPE) == 0) &&
        (input_sub_type.compare(ROLLCALL_REQUEST_SUB_TYPE) == 0))
    {
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
	agent->write(ROLLCALL_RESPONSE_TOPIC, output_message);
    }
}
