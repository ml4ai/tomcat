#include "AgentBase.hpp"
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/json.hpp>
#include <boost/log/trivial.hpp>

using namespace std;
namespace json = boost::json;
using namespace std::chrono;

/** Get current UTC timestamp in ISO-8601 format. */
string get_timestamp() {
    return boost::posix_time::to_iso_extended_string(
               boost::posix_time::microsec_clock::universal_time()) +
           "Z";
}


// 
AgentBase::AgentBase(json::object config): config(config) {
    string host = json::value_to<string>(config.at("host"));
    int port = json::value_to<int>(config.at("port"));

    // Create an MQTT client using a smart pointer to be shared among
    // threads.
    string address = "tcp://" + host + ": " + to_string(port);

    this->mqtt_client = make_shared<mqtt::async_client>(address, "agent");

    // Connect options for a non-persistent session and automatic
    // reconnects.
    auto connOpts = mqtt::connect_options_builder()
                        .clean_session(true)
                        .automatic_reconnect(seconds(2), seconds(30))
                        .finalize();

    mqtt_client->set_message_callback(
        [&](mqtt::const_message_ptr msg) { process(msg); });

    auto rsp = this->mqtt_client->connect(connOpts)->get_connect_response();
    BOOST_LOG_TRIVIAL(info) << "Connected to the MQTT broker at " << address;

    // Subscribe to the config subscriptions
    // TODO
    //mqtt_client->subscribe(input_topic, 2);

    // Start publishing heartbeat messages 
    heartbeat_future = async(launch::async, &AgentBase::publish_heartbeats, this);
}

/** Function that publishes heartbeat messages while the agent is running */
void AgentBase::publish_heartbeats() {
    while (this->running) {
        this_thread::sleep_for(seconds(10));

        string timestamp = get_timestamp();
	json::value jv = {{"header",
                           {{"timestamp", timestamp},
                            {"message_type", "status"},
                            {"version", "0.1"}}},
                          {"msg",
                           {{"timestamp", timestamp},
                            {"sub_type", "heartbeat"},
                            {"source", "tomcat-CDC"},
                            {"version", "0.0.1"}}},
                          {"data", {{"state", "ok"}}}};

        mqtt_client
            ->publish("status/tomcat-CDC/heartbeats", json::serialize(jv))
            ->wait();
    }
}

void AgentBase::stop() {
    running = false;
    heartbeat_future.wait();
}