#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include <set>

#include "HeartbeatProducer.hpp"
#include "Agent.hpp"

/* This class Publishes heartbeats on a beat interval */

using namespace std;
namespace json = boost::json;
using namespace std::chrono;

void HeartbeatProducer::configure(json::object config, Agent *agent) {

    MessageHandler::configure(config, agent);

    // spoof input message until we get a trial start
    json::object header;
    header["message_type"] = input_message_type;
    input_message["header"] = header;
    input_message["msg"] = json::object();
    input_message["data"] = json::object();

    // send a heartbeat immediately to advise we are in startup
    process_message(input_topic, input_message);
}

// copy the input for our async publications
void HeartbeatProducer::process_header(
    json::object input_message,
    json::object output_message,
    string timestamp
){
    this->input_message = input_message;

    MessageHandler::process_header(input_message, output_message, timestamp);
}

/** Function that publishes heartbeat messages while the agent is running */
void HeartbeatProducer::publish_heartbeats() {

    while (this->running) {
        this_thread::sleep_for(seconds(10));
	process_message(input_topic, input_message);
    }
}

// start publishing heartbeats
void HeartbeatProducer::start() {
    running = true;

    // if we're started we're good
    data["state"] = "ok";
    data["active"] = running;   
    data["status"] = "I am processing messages";

    // respond immediately
    process_message(input_topic, input_message);

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

    data["state"] = "ok";
    data["active"] = running;   
    data["status"] = "Stopped";

    // send feedback now
    process_message(input_topic, input_message);

    heartbeat_future.wait();
}
