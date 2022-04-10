#include "Agent.hpp"

using namespace std;
namespace json = boost::json;
using namespace std::chrono;

/** Get current UTC timestamp in ISO-8601 format. */
std::string get_timestamp() {
    return boost::posix_time::to_iso_extended_string(
               boost::posix_time::microsec_clock::universal_time()) +
           "Z";
}

Agent::Agent(std::string address) {
    // Create an MQTT client using a smart pointer to be shared among
    // threads.
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

    mqtt_client->subscribe("agent/dialog", 2);

    _future = std::async(std::launch::async, &Agent::publish_heartbeats, this);
    /** Start publishing heartbeat messages */
    // this->heartbeat_publisher = thread(&Agent::publish_heartbeats, this);
    //ĸkkk
};

/** Disconnect from the MQTT broker */
void Agent::disconnect() {
    BOOST_LOG_TRIVIAL(info) << "Disconnecting from MQTT broker...";
    this->mqtt_client->disconnect()->wait();
}

/** Function that publishes heartbeat messages while the agent is running */
void Agent::publish_heartbeats() {
    while (this->running) {
        this_thread::sleep_for(seconds(1));

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

        this->mqtt_client
            ->publish("status/tomcat-CDC/heartbeats", json::serialize(jv))
            ->wait();
    }
}

/** Destructor for the class that cleans up threads and disconnects from
 * the broker. */
Agent::~Agent() {
    this->running = false;
    _future.get();

    this->disconnect();
}