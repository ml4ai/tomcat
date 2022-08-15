#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include <set>

#include "HeartbeatProducer.hpp"
#include "Processor.hpp"
#include "Utils.hpp"

/* This class Publishes heartbeats on a beat interval */

using namespace std;
namespace json = boost::json;
using namespace std::chrono;

void HeartbeatProducer::configure(json::object config) {

    // input
    trial_start_config = utils.get_object("trial_start", config);
    trial_stop_config = utils.get_object("trial_stop", config);

    trial_topic = utils.get_string("topic", trial_start_config);
    cout << "HeartbeatProducer subscribing to " << trial_topic << endl;
    mqtt_client->subscribe(trial_topic, 2);

    // output
    heartbeat_config = utils.get_object("heartbeat", config);
    version_info_config = utils.get_object("version_info", config);
    heartbeat_topic = utils.get_string("topic", heartbeat_config);
    heartbeat_message_type = utils.get_string("message_type", heartbeat_config);
    heartbeat_sub_type = utils.get_string("sub_type", heartbeat_config);

    cout << "HeartbeatProducer publishing on " << heartbeat_topic << endl;

    // subscribe

    // the beat goes on
    start();
}

/* watch for trial start and stop messages */
void HeartbeatProducer::process_input_message(
    string topic,
    json::object input_message
) {
    if(trial_topic.compare(topic) != 0) {
        return;
    }

    json::object input_header = utils.get_object("header", input_message);
    json::object input_msg = utils.get_object("msg", input_message);
    json::object input_data = utils.get_object("data", input_message);

    cout << "HeartbeatProducer::process_input_message" << endl;
    if(utils.value_matches(trial_start_config, input_header, "message_type") &&
        utils.value_matches(trial_start_config, input_msg, "sub_type")) {
        cout << "HeartbeatProducer::trial start" << endl;
        this->input_header = input_header; // TODO make copies
        this->input_msg = input_msg;
	return;
    }
    if(utils.value_matches(trial_stop_config, input_header, "message_type") &&
        utils.value_matches(trial_stop_config, input_msg, "sub_type")) {
        cout << "HeartbeatProducer::trial stop" << endl;
        this->input_header = input_header; // TODO make copies
        this->input_msg = input_msg;
	return;
    }
}


// start the beat
void HeartbeatProducer::start() {
    running = true;

    cout << "HeartbeatProducer::start()" << endl;

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
            ->publish(heartbeat_topic, json::serialize(jv))
            ->wait();
    }
}

/** create the heartbeat message.  The msg object will have more fields 
 * when a trial is running */
json::value HeartbeatProducer::get_heartbeat() {

    string timestamp = utils.get_timestamp();

    json::value jv = {
        {"header",
            header(timestamp, heartbeat_message_type, input_header)},
        {"msg", 
            msg(timestamp, heartbeat_sub_type, input_msg)},
	{"data", {
            {"state", "not_set"}}}  // TODO set the state
    };

    return jv;
}

void HeartbeatProducer::stop() {
    running = false;
    heartbeat_future.wait();
}
