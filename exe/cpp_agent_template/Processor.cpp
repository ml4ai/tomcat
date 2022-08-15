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

    this->mqtt_client = mqtt_client;

    // get configuration for reading from the Message Bus if 
    // subscription name nonempty, otherwise skip 
    string sub_name = get_subscription_name();
    if(!sub_name.empty() &&
        !utils.parse_configuration(sub_name, config, &input_config)) {
        cerr << sub_name << " configuration parse error" << endl;
	exit(EXIT_FAILURE);
    }

    // get configuration for writing to the Message Bus if 
    // subscription name nonempty, otherwise skip 
    string pub_name = get_publication_name();
    if(!pub_name.empty() &&
        !utils.parse_configuration(pub_name, config, &output_config)) {
        cerr << pub_name << " configuration parse error" << endl;
	exit(EXIT_FAILURE);
    }

    // this software version
    if(config.contains("version")){
        version = json::value_to<std::string>(config.at("version"));
    } else {
        cerr << "Configuration missing 'version' field" << endl;
	exit(EXIT_FAILURE);
    }

    // this software publication source 
    if(config.contains("publication_source")){
        source = 
            json::value_to<std::string>(config.at("publication_source"));
    } else {
        cerr << "Configuration missing 'source' field" << endl;
	exit(EXIT_FAILURE);
    }
}


/** Check that the message is valid json, has the header, msg, and data objects
 *  and that the topic, message_type and sub_type match our configuration.  If
 *  everything checks out, respond to the message */
void Processor::process_traffic(string m_topic, mqtt::const_message_ptr m_ptr){

    /* test that the topic matches the subscription configuration */
    if(m_topic.compare(input_config.topic) != 0) {
        return;
    }

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

    /* Get message components */
    json::object header = json::value_to<json::object>(message.at("header"));
    json::object msg = json::value_to<json::object>(message.at("msg"));
    json::object data = json::value_to<json::object>(message.at("data"));

    /* message_type and sub_type must match configuration for processing */
    if(!utils.value_matches(header, "message_type", input_config.message_type) 
        || !utils.value_matches(msg, "sub_type", input_config.sub_type)) 
    {
	return;
    }

    /* if the traffic message matches our config, process it as input */
    process_input_message(header, msg, data);
}


// create a common header struct based on Message Bus input
json::value Processor::header(string timestamp, json::object input_header) {

    string testbed_version = "1.0";
    if (input_header.contains("version")) { 
	testbed_version = json::value_to<string>(input_header.at("version"));
    }

    json::value header = {
        {"timestamp", timestamp},
        {"message_type", output_config.message_type},
        {"version", testbed_version}
    };

    return header;
}

// create a common msg struct based on Message Bus input 
json::value Processor::msg(string timestamp, json::object input_msg) {

    json::object msg;
    msg["timestamp"] = timestamp;
    msg["source"] = source;
    msg["sub_type"] = output_config.sub_type;
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

void Processor::publish(json::value jv) {
    cout << "Publishing on " << output_config.topic << endl;
    mqtt_client->publish(output_config.topic, json::serialize(jv));
}
