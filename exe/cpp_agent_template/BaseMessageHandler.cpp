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


// return the topics from 
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

// get common header without topic-specific fields set
json::object BaseMessageHandler::get_output_header(
		const json::object &input_header,
                const string timestamp) {

    json::object output_header;
    output_header["timestamp"] = timestamp;
    output_header["version"] = 
        val_or_else<string>(input_header, "version", "1.0");

    return output_header;
}

// get common msg without topic-specific fields set
json::object BaseMessageHandler::get_output_msg(const json::object &input_msg,
                                                const string timestamp) {

    json::object output_msg;
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

void BaseMessageHandler::process_message(const string topic,
                                         const json::object &input_message) {

    json::object input_header = val<json::object>(input_message,"header");
    json::object input_msg = val<json::object>(input_message,"msg");
    string input_message_type = val<string>(input_header, "message_type");
    string in_sub_type = val<string>(input_msg, "sub_type");

    string timestamp = get_timestamp();


    // if trial start send version info
    if((topic.compare(TRIAL_TOPIC) == 0) &&
        (input_message_type.compare(TRIAL_MESSAGE_TYPE) == 0) &&
        (in_sub_type.compare(TRIAL_SUB_TYPE_START) == 0))
    {
        json::object output_header = get_output_header(input_header, timestamp);
	output_header["message_type"] = VERSION_INFO_MESSAGE_TYPE;

        json::object output_msg = get_output_msg(input_msg, timestamp);
	output_msg["sub_type"] = VERSION_INFO_SUB_TYPE;

	json::object output_message;
	output_message["header"] = output_header;
	output_message["msg"] = output_msg;
	output_message["data"] = version_info_data;

	agent->write(VERSION_INFO_TOPIC, output_message);
    }

    // if rollcall request send rollcall response
    else if((topic.compare(ROLLCALL_REQUEST_TOPIC) == 0) &&
        (input_message_type.compare(ROLLCALL_REQUEST_MESSAGE_TYPE) == 0) &&
        (in_sub_type.compare(ROLLCALL_REQUEST_SUB_TYPE) == 0))
    {
        json::object output_header = 
            get_output_header(input_message, timestamp);
	output_header["message_type"] = ROLLCALL_RESPONSE_MESSAGE_TYPE;

        json::object output_msg = get_output_msg(input_message, timestamp);
	output_msg["sub_type"] = ROLLCALL_RESPONSE_SUB_TYPE;

        json::object input_data = val<json::object>(input_message,"data");

	time_t now;
	time(&now);

	int uptime = now-start_time;

	json::object out_data;
	out_data["version"] = version;
	out_data["uptime"] = uptime;
	out_data["status"] = "up";
	out_data["rollcall_id"] = 
            val_or_else<string>(input_data, "rollcall_id", "not set");

	json::object output_message;
	output_message["header"] = output_header;
	output_message["msg"] = output_msg;
	output_message["data"] = out_data;

	agent->write(ROLLCALL_RESPONSE_TOPIC, output_message);
    }
}
