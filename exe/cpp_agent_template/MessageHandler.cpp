#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include <boost/json/array.hpp>
#include <mqtt/async_client.h>
#include <boost/date_time/posix_time/posix_time.hpp>

#include "Agent.hpp"

#include "MessageHandler.hpp"

using namespace std;
namespace json = boost::json;


MessageHandler::MessageHandler(Agent *agent): agent(agent) {
}



/** Get current UTC timestamp in ISO-8601 format. */
string MessageHandler::get_timestamp() {
    return boost::posix_time::to_iso_extended_string(
        boost::posix_time::microsec_clock::universal_time()
    ) + "Z";
}


// Set parameters using the configuration
void MessageHandler::configure( json::object config) {

    version_info_data = config;   // TODO Will want this class info

    input_topics.push_back("agent/control/rollcall/request");
    input_topics.push_back("trial");

    version = val_or_else<string>(config, "version", "not set");
    source = val_or_else<string>(config, "agent_name", "not set");
}

void MessageHandler::process_message(string topic, json::object in_message) {
    json::object in_header = val<json::object>(in_message,"header");
    json::object in_msg = val<json::object>(in_message,"msg");
    json::object in_data = val<json::object>(in_message,"data");
    string in_message_type = val<string>(in_header, "message_type");
    string in_sub_type = val<string>(in_msg, "sub_type");

    string timestamp = get_timestamp();

    json::object out_header;
    out_header["timestamp"] = timestamp;
    out_header["version"] = val_or_else<string>(in_header, "version", "1.0");

    json::object out_msg;
    out_msg["source"] = source;
    out_msg["version"] = version;
    out_msg["timestamp"] = timestamp;

    // add these if they exist and are non-empty
    if(!val<string>(in_msg, "experiment_id").empty()) {
        out_msg["experiment_id"] = in_msg.at("experiment_id");
    }
    if(!val<string>(in_msg, "trial_id").empty()) {
        out_msg["trial_id"] = in_msg.at("trial_id");
    }
    if(!val<string>(in_msg, "replay_id").empty()) {
        out_msg["replay_id"] = in_msg.at("replay_id");
    }
    if(!val<string>(in_msg, "replay_root_id").empty()) {
        out_msg["replay_root_id"] = in_msg.at("replay_root_id");
    }

    // if trial start send version info
    if((string("trial").compare(topic) == 0) &&
        (string("trial").compare(in_message_type) == 0) &&
        (string("start").compare(in_sub_type) == 0))
    {
	out_header["message_type"] = "agent";
	out_msg["sub_type"] = "versioninfo";

	json::object out_message;
	out_message["header"] = out_header;
	out_message["msg"] = out_msg;
	out_message["data"] = version_info_data;

	agent->write("agent/reference_agent/versioninfo", out_message);
    }

    // if rollcall request send rollcall response
    if((string("agent/control/rollcall/request").compare(topic) == 0) &&
        (string("agent").compare(in_message_type) == 0) &&
        (string("rollcall:request").compare(in_sub_type) == 0))
    {
	out_header["message_type"] = "agent";
	out_msg["sub_type"] = "rollcall:response";

	json::object out_data;
	out_data["version"] = version;
	out_data["uptime"] = 1234; // TODO get real
	out_data["status"] = "up";
	out_data["rollcall_id"] = 
            val_or_else<string>(in_data, "rollcall_id", "not set");

	json::object out_message;
	out_message["header"] = out_header;
	out_message["msg"] = out_msg;
	out_message["data"] = out_data;

	agent->write("agent/reference_agent/versioninfo", out_message);
    }
}
