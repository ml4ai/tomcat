#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include <set>

#include "HeartbeatProducer.hpp"

/* This class Publishes heartbeats on a beat interval */

using namespace std;
namespace json = boost::json;
using namespace std::chrono;

void HeartbeatProducer::configure(
    json::object config,
    std::shared_ptr<mqtt::async_client> mqtt_client
) {

    MessageHandler::configure(config, mqtt_client);

    data["state"] = "Configuring";
    data["active"] = running;   
    data["status"] = "ok";

    // we don't have message bus input at startup so we have to spoof some
    json::object phony;
    json::object header;
    header["message_type"] = input_message_type;
    phony["header"] = header;

    // send a heartbeat immediately to advise we are in startup
    process_message(phony);
}

// copy the input for our async publications
void HeartbeatProducer::process_message(json::object input_message) {
    this->input_message = input_message;

    MessageHandler::process_message(input_message);
}

/** Function that publishes heartbeat messages while the agent is running */
void HeartbeatProducer::publish_heartbeats() {

    while (this->running) {
        this_thread::sleep_for(seconds(10));
	process_message(input_message);
    }
}

// start publishing heartbeats
void HeartbeatProducer::start() {
    running = true;

    // if we're started we're good
    data["state"] = "ok";
    data["active"] = running;   
    data["status"] = "I am processing messages";

    // send feedback now
    process_message(input_message);

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
    process_message(input_message);

    heartbeat_future.wait();
}
