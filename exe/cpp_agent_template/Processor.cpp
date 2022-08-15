#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include <mqtt/async_client.h>

#include "Processor.hpp"
#include "Coordinator.hpp"
#include "Utils.hpp"

using namespace std;
namespace json = boost::json;


/** Read the the topic, header.message_type, and msg.sub_type fields
 *  from the configuration.  The processor can't run without these 
 *  values */
void Processor::configure(
    json::object config,
    std::shared_ptr<mqtt::async_client> mqtt_client
) {
    this->config = config;
    this->mqtt_client = mqtt_client;

    // this software version
    version = utils.get_string("version", config);

    // this software publication source 
    source = utils.get_string("publication_source", config);

    // extending classes configure now
    configure(config);
}


/** Check that the message is valid json, has the header, msg, and data objects
 *  and that the topic, message_type and sub_type match our configuration.  If
 *  everything checks out, respond to the message */
void Processor::process_message(
    string m_topic,
    mqtt::const_message_ptr m_ptr
){

    /* convert m_prt to a JSON message  */
    json_parser.reset();
    error_code ec;
    json_parser.write(m_ptr->get_payload_str(), ec);
    if(ec) {
        cerr << "msg.topic: " << m_topic << endl;
        cerr << "JSON parse error code: " << ec << endl;
        return;
    }
    json::object message = json::value_to<json::object>(json_parser.release());

    /* process message as input */
    process_input_message(m_topic, message);
}

// create an output message common header
json::value Processor::header(
    string timestamp,
    string output_message_type,
    json::object input_header) 
{
    string testbed_version = utils.get_string("version", input_header);
    if (testbed_version.empty()) {
        testbed_version = "1.0";
    }

    json::value header = {
        {"timestamp", timestamp},
        {"message_type", output_message_type},
        {"version", testbed_version}
    };

    return header;
}

// create an output message common msg
json::value Processor::msg(
        string timestamp, 
        string output_sub_type, 
        json::object input_msg
) {
    json::object msg;
    msg["timestamp"] = timestamp;
    msg["source"] = source;
    msg["sub_type"] = output_sub_type;
    msg["version"] = version;

    // msg fields that may or may not be present
    if(input_msg.contains("experiment_id")) {
        msg["experiment_id"] = input_msg.at("experiment_id");
    }
    if(input_msg.contains("trial_id")) {
        msg["trial_id"] = input_msg.at("trial_id");
    }
    if(input_msg.contains("replay_root_id")) {
        msg["replay_root_id"] = input_msg.at("replay_root_id");
    }
    if(input_msg.contains("replay_id")) {
        msg["replay_id"] = input_msg.at("replay_id");
    }

    return msg;
}

void Processor::publish(string topic, json::value jv) {
    cout << "Publishing on " << topic << endl;
    mqtt_client->publish(topic, json::serialize(jv));
}
