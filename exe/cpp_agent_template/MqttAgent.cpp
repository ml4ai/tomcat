#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "Agent.hpp"
#include "MqttAgent.hpp"
#include <iostream>


// This class :
//   Maintains the MQTT broker connection
//

using namespace std;
namespace json = boost::json;
using namespace std::chrono;


MqttAgent::MqttAgent(const json::object &config) : Agent(config) {


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

	read(topic, message);
    });

    auto rsp = this->mqtt_client->connect(connOpts)->get_connect_response();
    BOOST_LOG_TRIVIAL(info) << "Connected to the MQTT broker at " << address;

    // advise of subscribed topics
    cout << "Subscription topics:" << endl;
    for(string i : get_read_topics()) {
        mqtt_client->subscribe(i, 2);
	cout << "    " << i << endl;
    }

    // advise of published topics
    cout << "Publication topics:" << endl;
    for(string i : get_write_topics()) {
	cout << "    " << i << endl;
    }

    start();
}

void MqttAgent::write(const string topic, json::object &message) {
    cout << "MqttAgent::write on " << topic << ": " << message << endl;
    mqtt_client->publish(topic, json::serialize(message));
}

void MqttAgent::start() {

}

void MqttAgent::stop() {

}
