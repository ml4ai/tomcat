#include "MinecraftMQTT2LSL.h"

#include <iostream>

#include <fmt/format.h>
#include <nlohmann/json.hpp>

#include "data_stream/Mosquitto.h"

using namespace std;
using namespace nlohmann;

//----------------------------------------------------------------------
// Constructors & Destructor
//----------------------------------------------------------------------
MinecraftMQTT2LSL::MinecraftMQTT2LSL() {
    this->minecraft_lsl_stream =
        make_unique<LSLStringStream>("Minecraft", "minecraft", "minecraft");
}

//----------------------------------------------------------------------
// Other functions
//----------------------------------------------------------------------
void MinecraftMQTT2LSL::start(const string& mqtt_address,
                              int mqtt_port,
                              std::atomic<bool>* signal_watcher) {
    Mosquitto mosquitto = Mosquitto();

    mosquitto.connect(mqtt_address, mqtt_port);
    auto callback_fn = [this](const std::string& t, const std::string& m) {
        this->push_to_lsl(t, m);
    };
    mosquitto.set_on_message_callback(callback_fn);
    // Minecraft is the only system pushing to the message bus at the moment.
    // The list of topics is very large and setting them manually would require
    // to update the list in this program as well at each study upgrade. So we
    // just push all the topics to LSL.
    mosquitto.subscribe("#");

    this->minecraft_lsl_stream->open();
    mosquitto.start();

    while (!signal_watcher->load()) {
        // Do nothing. The MQTT loop thread will be active and running
        // at this moment. When the variable signal_watcher changes to true by
        // a program interruption, we leave this loop and stop the watcher.
    }

    mosquitto.stop();
}

void MinecraftMQTT2LSL::push_to_lsl(const std::string& topic,
                                    const std::string& message) {
    string output_data;
    try {
        json msg_json = json::parse(message);
        msg_json["topic"] = topic;
        output_data = msg_json.dump();
    }
    catch (...) {
        // Invalid json. We pass the topic and the message as is.
        output_data = fmt::format("[{}] {}", topic, message);
    }

    this->minecraft_lsl_stream->send(output_data);
}
