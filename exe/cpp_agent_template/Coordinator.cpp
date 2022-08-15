#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "Coordinator.hpp"
#include "Processor.hpp"

/* This class :
 *   Maintains the MQTT broker connection
 *   Publishes heartbeats on a beat interval
 *   Dispatches messages to their respective handlers
 */

using namespace std;
namespace json = boost::json;
using namespace std::chrono;


Coordinator::Coordinator(json::object config) {

    this->config = config;

    // set up MQTT params for broker connection
    json::object mqtt_config = json::value_to<json::object>(config.at("mqtt"));
    string host = json::value_to<string>(mqtt_config.at("host"));
    int port = json::value_to<int>(mqtt_config.at("port"));
    string address = "tcp://" + host + ": " + to_string(port);

    // Create an MQTT client smart pointer to be shared among threads.
    this->mqtt_client = make_shared<mqtt::async_client>(
        address, 
	"cpp_template_agent"
    );

    // Connect options for a non-persistent session and automatic
    // reconnects.
    auto connOpts = mqtt::connect_options_builder()
                        .clean_session(true)
                        .automatic_reconnect(seconds(2), seconds(30))
                        .finalize();

    mqtt_client->set_message_callback([&](mqtt::const_message_ptr msg) {
        for(int i = 0; i < N_PROCESSORS; i ++) {
            processors[i]->process_message(msg->get_topic(), msg);
        }
    });

    auto rsp = this->mqtt_client->connect(connOpts)->get_connect_response();
    BOOST_LOG_TRIVIAL(info) << "Connected to the MQTT broker at " << address;

    // configure message processors
    for(int i = 0; i < N_PROCESSORS; i ++) {
        processors[i]->configure(config, mqtt_client);
    }
}

void Coordinator::stop() {
    heartbeat_producer.stop();
}
