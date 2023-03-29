#include "MinecraftMQTT2LSL.h"

#include <iostream>

#include <nlohmann/json.hpp>
#include <fmt/format.h>

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
void MinecraftMQTT2LSL::start(const string& mqtt_address, int mqtt_port) {
    Mosquitto mosquitto = Mosquitto();

    mosquitto.connect(mqtt_address, mqtt_port);
    auto callback_fn = [this](const std::string& t, const std::string& m) {
        this->push_to_lsl(t, m);
    };
    mosquitto.set_on_message_callback(callback_fn);
    mosquitto.subscribe("#");
    mosquitto.loop_forever();
}

void MinecraftMQTT2LSL::push_to_lsl(const std::string& topic,
                                    const std::string& message) {
    string output_data;
    try {
        json msg_json = json::parse(message);
        msg_json["topic"] = topic;
        output_data = msg_json.dump();
    } catch (...) {
        // Invalid json. We pass the topic and the message as is.
        output_data = fmt::format("[{}] {}", topic, message);
    }

    cout << output_data << endl;
    this->minecraft_lsl_stream->send(message);
}
