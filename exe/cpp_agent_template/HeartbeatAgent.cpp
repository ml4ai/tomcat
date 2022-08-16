#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include <set>

#include "HeartbeatAgent.hpp"
#include "Processor.hpp"
#include "Utils.hpp"

/* This class Publishes heartbeats on a beat interval */

using namespace std;
namespace json = boost::json;
using namespace std::chrono;

json::object HeartbeatAgent::get_input_config(json::object config){
    return get_object("trial_start", config);
}

json::object HeartbeatAgent::get_output_config(json::object config){
    return get_object("version_info", config);
}

json::object HeartbeatAgent::get_output_data(json::object input_data){
    json::object output_data;
    output_data["heartbeat_data"] = "goes here";

    return output_data;
}



/*
void HeartbeatAgent::process_input_message(
    string topic,
    json::object input_message
) {
    if(input_topic.compare(topic) != 0) {
        return;
    }

    json::object input_header = utils.get_object("header", input_message);
    json::object input_msg = utils.get_object("msg", input_message);

    cout << "HeartbeatAgent::process_input_message" << endl;
    if(trial_start_config.at("message_type") != input_header.at("message_type"))
    {
        return;
    }
    // trial start
    if(trial_start_config.at("sub_type") == input_msg.at("sub_type")){
        cout << "HeartbeatAgent::trial start" << endl;
        this->input_header = input_header; // TODO make copies
        this->input_msg = input_msg;
	return;
    }
    // trial stop
    if(trial_stop_config.at("sub_type") == input_msg.at("sub_type")){
        cout << "HeartbeatAgent::trial stop" << endl;
        this->input_header = input_header; // TODO make copies
        this->input_msg = input_msg;
	return;
    }
}
*/


// start the beat
void HeartbeatAgent::start() {
    running = true;

    cout << "HeartbeatAgent::start()" << endl;

    // Start publishing heartbeat messages 
    heartbeat_future = async(
        launch::async, 
	&HeartbeatAgent::publish_heartbeats, 
	this
    );
}


/** Function that publishes heartbeat messages while the agent is running */
void HeartbeatAgent::publish_heartbeats() {
    while (this->running) {
        this_thread::sleep_for(seconds(10));
	json::value jv = get_heartbeat();

        mqtt_client
            ->publish(output_topic, json::serialize(jv))
            ->wait();
    }
}

json::value HeartbeatAgent::get_heartbeat(){

    json::object heartbeat_message;
    heartbeat_message["heartbeat_message"] = "goes here";

    return heartbeat_message;
}

void HeartbeatAgent::stop() {
    running = false;
    heartbeat_future.wait();
}
