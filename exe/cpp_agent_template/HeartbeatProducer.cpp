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

    string input_config_name = get_input_config_name();
    json::object input_config = get_value<json::object>("trial_start", config);
    json::object output_config = get_value<json::object>("heartbeat", config);


    string message_type = get_value<string>("message_type", output_config);
    heartbeat_header["timestamp"] = "not set";
    heartbeat_header["message_type"] =
        get_value<string>("message_type", input_config);
    heartbeat_header["version"] = 1.0;

    heartbeat_msg["timestamp"] = "not set";
    heartbeat_msg["source"] = get_value<string>("agent_name", config);
    heartbeat_msg["version"] = get_value<string>("version", config);

    heartbeat_data["state"] = "Configuring";
    heartbeat_data["active"] = running;   
    heartbeat_data["status"] = "ok";

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

    cout << "HeartbeatProducer::process_json_message" << endl;
    cout << "Input header: " << input_header << endl;
    cout << "Input msg: " << input_msg << endl;

    // publish a heartbeat right away
    publish_heartbeat();
}

// start the beat
void HeartbeatProducer::start() {
    running = true;

    heartbeat_data["state"] = "ok";
    heartbeat_data["active"] = running;   
    heartbeat_data["status"] = "I am processing messages";

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
    json::object output_message = get_message(
        input_header,
	input_msg,
	json::object()
    );

    return output_message;
}

json::object HeartbeatProducer::get_message(
    json::object input_header,
    json::object input_msg,
    json::object input_data)
{
    string ts = get_timestamp();

    cout << "HeartbeatProducer::get_message" << endl;
    cout << "Input header: " << input_header << endl;
    cout << "Input msg: " << input_msg << endl;

    json::object message;
    message["header"] = get_header(heartbeat_header, input_header, ts);
    message["msg"] = get_msg(heartbeat_msg, input_msg, ts);
    message["data"] = heartbeat_data;

    return message;
}


void HeartbeatProducer::stop() {
    running = false;
    heartbeat_future.wait();
}
