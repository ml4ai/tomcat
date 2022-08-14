#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include <set>

#include "HeartbeatProducer.hpp"
#include "Processor.hpp"
#include "Utils.hpp"

/* This class :
 *   Maintains the MQTT broker connection
 *   Publishes heartbeats on a beat interval
 *   Dispatches messages to their respective handlers
 */

using namespace std;
namespace json = boost::json;
using namespace std::chrono;


// 
// prepare for running
void HeartbeatProducer::configure(
    json::object config, 
    std::shared_ptr<mqtt::async_client> mqtt_client
) {

    this->mqtt_client = mqtt_client;

    /** get publication configuration */
    string pub_name = "heartbeat";
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

    // Start publishing heartbeat messages 
    heartbeat_future = async(
        launch::async, 
	&HeartbeatProducer::publish_heartbeats, 
	this
    );
}


/** Function that publishes heartbeat messages while the agent is running */
void HeartbeatProducer::publish_heartbeats() {
    while (this->running) {
        this_thread::sleep_for(seconds(10));
	json::value jv = get_heartbeat();

        mqtt_client
            ->publish(pub_config.topic, json::serialize(jv))
            ->wait();
    }
}

/** create the heartbeat message.  The msg object will have more fields 
 * when a trial is running */
json::value HeartbeatProducer::get_heartbeat() {

    string timestamp = utils.get_timestamp();

    json::value jv = {
        {"header",  {
            {"message_type",pub_config.message_type},
            {"timestamp", timestamp},
            {"version", "0.1"}}},
        {"msg",  {
            {"sub_type",pub_config.sub_type},
            {"timestamp", timestamp},
            {"source",source},
            {"version",version}}},
	{"data", {
            {"state", "not_set"}}}
    };

    return jv;
}

void HeartbeatProducer::stop() {
    running = false;
    heartbeat_future.wait();
}
