#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include <set>

#include "HeartbeatProducer.hpp"

/* This class Publishes heartbeats on a beat interval */

using namespace std;
namespace json = boost::json;
using namespace std::chrono;

HeartbeatProducer::HeartbeatProducer() {
    output_data["state"] = "ok";
    output_data["active"] = running;   
    output_data["status"] = "not configured";
}

json::object HeartbeatProducer::create_output_data(json::object input_data){
    return output_data;
}

// set the status vars.  Updated heartbeat is published immediately
void HeartbeatProducer::set_status(string state, bool active, string status) {

    running = active;
    
    output_data["state"] = state;
    output_data["active"] = running;   
    output_data["status"] = status;

    publish_heartbeat();
}

// we override this method because we have to watch the trial comms differently
void HeartbeatProducer::process_json_message(json::object json_message){

    // header.message_type must match our configuration
    input_header = get_value<json::object>("header", json_message);
    string message_type = get_value<string>("message_type", input_header);
    if(input_message_type.compare(message_type)) {
        return;
    }

    // We don't branch on sub_type.  

    // The header and msg objects from any trial message are recorded and
    // used for subsequent heartbeat messages.
    input_msg = get_value<json::object>("msg", json_message);

    // publish a heartbeat right away
    publish_heartbeat();
}

// start the beat
void HeartbeatProducer::start() {
    running = true;

    set_status("ok", running, "I am processing messages");

    // Start publishing on the beat
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
            ->publish(output_topic, json::serialize(jv))
            ->wait();
    }
}

// publish an immediate heartbeat
void HeartbeatProducer::publish_heartbeat() {
    publish(get_heartbeat());
}

json::object HeartbeatProducer::get_heartbeat(){

    // compose output message
    json::object output_message = create_output_message(
        input_header,
	input_msg,
	json::object()
    );

    return output_message;
}

void HeartbeatProducer::stop() {
    running = false;
    heartbeat_future.wait();
}
