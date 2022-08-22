#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include <set>
#include <iostream>

#include "HeartbeatProducer.hpp"
#include "Agent.hpp"

/* This class Publishes heartbeats on a beat interval */

using namespace std;
namespace json = boost::json;
using namespace std::chrono;


void HeartbeatProducer::configure(const json::object &config) {

    // initial state
    output_data["state"] = "ok";
    output_data["active"] = running;   
    output_data["status"] = "Initializing";
}

/** Function that publishes heartbeat messages while the agent is running */
void HeartbeatProducer::publish_heartbeats() {

    while (running) {
        this_thread::sleep_for(seconds(10));
	publish_heartbeat();
    }
}

void HeartbeatProducer::publish_heartbeat() {
    string timestamp = get_timestamp();

    // create common header
    json::object output_header = create_output_header(
        input_message,
        timestamp,
        HEARTBEAT_MESSAGE_TYPE
    );

    // create common msg
    json::object output_msg = create_output_msg(
        input_message,
        timestamp,
        HEARTBEAT_SUB_TYPE
    );
    
    // data is already defined

    // assemble outgoing message
    json::object output_message;
    output_message["header"] = output_header;
    output_message["msg"] = output_msg;
    output_message["data"] = output_data;

    // agent takes it from here
    agent->publish(HEARTBEAT_TOPIC, output_message);
}

// start publishing heartbeats
void HeartbeatProducer::start() {
    running = true;

    // Running state
    output_data["state"] = "ok";
    output_data["active"] = running;   
    output_data["status"] = "I am processing messages";

    // send immediate ack
    publish_heartbeat();

    // Start threaded publishing
    heartbeat_future = async(
        launch::async, 
	&HeartbeatProducer::publish_heartbeats, 
	this
    );
}

// stop publishing heartbeats
void HeartbeatProducer::stop() {
    running = false;

    // stopped state
    output_data["state"] = "ok";
    output_data["active"] = running;   
    output_data["status"] = "Stopped";

    // send immediate ack
    publish_heartbeat();

    heartbeat_future.wait();
}

// Process trial start and stop messages
void HeartbeatProducer::process_message(const string topic,
                                         const json::object &input_message) {

    string input_message_type = get_message_type(input_message);
    string input_sub_type = get_sub_type(input_message);

    if((topic.compare(TRIAL_TOPIC) != 0) &&
        (input_message_type.compare(TRIAL_MESSAGE_TYPE) == 0) &&
        ((input_sub_type.compare(TRIAL_SUB_TYPE_START) == 0) ||
        (input_sub_type.compare(TRIAL_SUB_TYPE_STOP) == 0))) 
    {
        this->input_message = input_message;

	// send immediate ack
	publish_heartbeat();
    }
}
