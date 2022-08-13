#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include <mqtt/async_client.h>

#include "Processor.hpp"
#include "Utils.hpp"

using namespace std;
namespace json = boost::json;


/** Read the the topic, header.message_type, and msg.sub_type fields
 *  from the configuration.  The processor can't run without these 
 *  values */
void Processor::configure(json::object config){
    // use the configuration object that matches our hard-coded name

    string sub_name = get_subscription_name();
    if(!utils.parse_configuration(sub_name, config, &sub_config)) {
        cout << sub_name << " configuration parse error" << endl;
	exit(EXIT_FAILURE);
    }

    string pub_name = get_publication_name();
    if(!utils.parse_configuration(pub_name, config, &pub_config)) {
        cout << pub_name << " configuration parse error" << endl;
	exit(EXIT_FAILURE);
    }
}


/** Check that the message is valid json, has the header, msg, and data objects
 *  and that the topic, message_type and sub_type match our configuration.  If
 *  everything checks out, respond to the message */
void Processor::process(mqtt::const_message_ptr m_ptr){

    /* test that the topic matches the subscription configuration */
    string m_topic = m_ptr->get_topic();
    if(m_topic.compare(sub_config.topic) != 0) {
        return;
    }

    /* convert the message to a JSON object  */
    json_parser.reset();
    error_code ec;
    json_parser.write(m_ptr->get_payload_str(), ec);
    if(ec) {
        cout << "msg.topic: " << m_topic << endl;
        cout << "JSON parse error code: " << ec << endl;
 	return;
    }
    json::object message = json::value_to<json::object>(json_parser.release());
   
    /* get the message header object */
    if(!message.contains("header")) {
        cout << "Message has no header object" << endl;
        return;
    }
    json::object header = json::value_to<json::object>(message.at("header"));

    /* test that the header message_type matches the configuration */
    if(!utils.value_matches(header, "message_type", sub_config.message_type)) {
	return;
    }

    /* get the msg object and test that it is ours */
    if(!message.contains("msg")) {
        cout << "Message has no msg object" << endl;
        return;
    }
    json::object msg = json::value_to<json::object>(message.at("msg"));

    /* test that the msg sub_type matches the configuration */
    if(!utils.value_matches(msg, "sub_type", sub_config.sub_type)) {
	return;
    }

    /* get the data object */
    if(!message.contains("data")) {
        cout << "Message has no data object" << endl;
        return;
    }
    json::object data = json::value_to<json::object>(message.at("data"));

    /* If all the message objects exist and match the configuration, process
     * the JSON message */
    process(header, msg, data);
}
