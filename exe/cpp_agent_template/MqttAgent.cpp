#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "MqttAgent.hpp"

// This class :
//   Maintains the MQTT broker connection
//

using namespace std;
namespace json = boost::json;
using namespace std::chrono;


MqttAgent::MqttAgent(json::object config) : Agent() {

    configure(config);

    // set up MQTT params for broker connection
    json::object mqtt_config = json::value_to<json::object>(config.at("mqtt"));
    string host = json::value_to<string>(mqtt_config.at("host"));
    int port = json::value_to<int>(mqtt_config.at("port"));
    string address = "tcp://" + host + ": " + to_string(port);

    // Create an MQTT client smart pointer to be shared among threads.
    mqtt_client = make_shared<mqtt::async_client>(
        address, 
	"cpp_template_agent"
    );

    // Connect options for a non-persistent session and automatic
    // reconnects.
    auto connOpts = mqtt::connect_options_builder()
                        .clean_session(true)
                        .automatic_reconnect(seconds(2), seconds(30))
                        .finalize();

    mqtt_client->set_message_callback([&](mqtt::const_message_ptr m_ptr) {

	string topic = m_ptr->get_topic();

        // payload must be valid JSON
        json_parser.reset();
        error_code ec;
        json_parser.write(m_ptr->get_payload_str(), ec);
        if(ec) {
            cerr << "Error reading form topic: " << topic << endl;
            cerr << "Message is not valid JSON." << endl;
            cerr << "JSON parse error code: " << ec << endl;
            return;
        }

	// create json message
        json::object message = 
	    json::value_to<json::object>(json_parser.release());

	process_message(topic, message);
    });

    auto rsp = this->mqtt_client->connect(connOpts)->get_connect_response();
    BOOST_LOG_TRIVIAL(info) << "Connected to the MQTT broker at " << address;

    // Subscribe to input topics
    for(string topic : get_input_topics()) {
        mqtt_client->subscribe(topic, 2);
        cout << "Subscribed to: " << topic << endl;
    }

    // report output topics
    for(string topic : get_output_topics()) {
        cout << "Publishing on: " << topic << endl;
    }

    start();
}


void MqttAgent::write(string topic, json::object message) {
    mqtt_client->publish(topic, json::serialize(message));
}

