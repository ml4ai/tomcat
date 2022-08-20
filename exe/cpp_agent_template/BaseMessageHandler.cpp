#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include <boost/json/array.hpp>
#include <mqtt/async_client.h>
#include <boost/date_time/posix_time/posix_time.hpp>

#include "Agent.hpp"

#include "BaseMessageHandler.hpp"

using namespace std;
namespace json = boost::json;

/*
{
  "agent_name": "AC_UAZ_TA1_ReferenceAgent",
  "owner": "University of Arizona",
  "version": "1.0.0",
  "config": [],
  "dependencies": [],
  "source": [
    "https://gitlab.asist.aptima.com:5050/asist/testbed/AC_UAZ_TA1_ReferenceAgent:i.j.k"
  ],
  "publishes": [
    {
      "topic": "agent/reference_agent_output",
      "message_type": "reference_agent_output_message_type",
      "sub_type": "reference_agent_output_sub_type"
    },
    {
      "topic": "agent/control/rollcall/response",
      "message_type": "agent",
      "sub_type": "rollcall:response"
    },
    {
      "topic": "status/reference_agent/heartbeats",
      "message_type": "status",
      "sub_type": "heartbeat"
    },
    {
      "topic": "agent/reference_agent/versioninfo",
      "message_type": "agent",
      "sub_type": "versioninfo"
    }
  ],
  "subscribes": [
    {
      "topic": "agent/reference_agent_input",
      "message_type": "reference_agent_input_message_type",
      "sub_type": "reference_agent_input_sub_type"
    },
    {
      "topic": "agent/control/rollcall/request",
      "message_type": "agent",
      "sub_type": "rollcall:request"
    },
    {
      "topic": "trial",
      "message_type": "trial",
      "sub_type": "start"
    },
    {
      "topic": "trial",
      "message_type": "trial",
      "sub_type": "stop"
    }
  ]
}
*/

BaseMessageHandler::BaseMessageHandler(Agent *agent,
                               const json::object &config): agent(agent) 
{
    version_info_data = config;   // TODO Will want this class info

    version = val_or_else<string>(config, "version", "not set");
    source = val_or_else<string>(config, "agent_name", "not set");
}

vector<string>BaseMessageHandler::get_input_topics() {
    vector<string> input_topics;
    input_topics.push_back("agent/control/rollcall/request");
    input_topics.push_back("trial");
    return input_topics;
}

vector<string>BaseMessageHandler::get_output_topics() {
    vector<string> output_topics;
    output_topics.push_back("agent/reference_agent/versioninfo");
    output_topics.push_back("status/reference_agent/heartbeats");
    output_topics.push_back("agent/control/rollcall/response");
    return output_topics;
}

/** Get current UTC timestamp in ISO-8601 format. */
string BaseMessageHandler::get_timestamp() {
    return boost::posix_time::to_iso_extended_string(
        boost::posix_time::microsec_clock::universal_time()
    ) + "Z";
}


void BaseMessageHandler::process_message(const string topic,
                                         const json::object &in_message) {
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
