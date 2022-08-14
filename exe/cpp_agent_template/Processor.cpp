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

    // get configuration for reading from the Message Bus
    string sub_name = get_subscription_name();
    if(!utils.parse_configuration(sub_name, config, &sub_config)) {
        cerr << sub_name << " configuration parse error" << endl;
	exit(EXIT_FAILURE);
    }

    // get configuration for writing to the Message Bus
    string pub_name = get_publication_name();
    if(!utils.parse_configuration(pub_name, config, &pub_config)) {
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
    if(m_topic.compare(sub_config.topic) != 0) {
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

    /* test message_type and sub_type match the subscription configuration */
    if(!utils.value_matches(header, "message_type", sub_config.message_type) ||
       !utils.value_matches(msg, "sub_type", sub_config.sub_type)) 
    {
	return;
    }

    /* if the traffic message matches our config, this is input data for us */
    process_input_message(header, msg, data);
}


// create the header component of a message to be published
// bus_header is from the message that was read from the Message Bus
json::value Processor::header(string timestamp, json::object bus_header) {

    string testbed_version = "1.0";
    if (bus_header.contains("version")) { 
	testbed_version = json::value_to<string>(bus_header.at("version"));
    }

    json::value header = {
        {"timestamp", timestamp},
        {"message_type", pub_config.message_type},
        {"version", testbed_version}
    };

    return header;
}

// create the msg component of a message to be published
// bus_msg is from the message that was read from the Message Bus
json::value Processor::msg(string timestamp, json::object bus_msg) {

    json::object msg;
    msg["experiment_id"] = bus_msg.at("experiment_id");
    msg["timestamp"] = timestamp;
    msg["source"] = source;
    msg["sub_type"] = pub_config.sub_type;
    msg["version"] = version;

    // msg fields that may or may not be present
    if(bus_msg.contains("trial_id")) {
        msg["trial_id"] = bus_msg.at("trial_id");
    }
    if(bus_msg.contains("replay_root_id")) {
        msg["replay_root_id"] = bus_msg.at("replay_root_id");
    }
    if(bus_msg.contains("replay_id")) {
        msg["replay_id"] = bus_msg.at("replay_id");
    }

    return msg;
}

void Processor::publish(json::value jv) {
    cout << "Publishing on " << pub_config.topic << endl;
    mqtt_client->publish(pub_config.topic, json::serialize(jv));
}
