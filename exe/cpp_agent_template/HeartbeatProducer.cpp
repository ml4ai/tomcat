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


/* Process the rollcall request input from the message bus */
void HeartbeatProducer::process_input_message(
    json::object input_header,
    json::object input_msg,
    json::object input_data
) {
    cout << "HeartbeatProducer::process_input_message" << endl;
}


// start the beat
void HeartbeatProducer::start() {
    running = true;

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

void HeartbeatProducer::set_input(
    json::object input_header, 
    json::object input_msg) 
{
    this->input_header = input_header; // TODO make copies
    this->input_msg = input_msg;
}

/** create the heartbeat message.  The msg object will have more fields 
 * when a trial is running */
json::value HeartbeatProducer::get_heartbeat() {

    string timestamp = utils.get_timestamp();

    json::value jv = {
        {"header",
            header(timestamp, input_header)},
        {"msg", 
            msg(timestamp, input_msg)},
	{"data", {
            {"state", "not_set"}}}  // TODO set the state
    };

    return jv;
}

void HeartbeatProducer::stop() {
    running = false;
    heartbeat_future.wait();
}
