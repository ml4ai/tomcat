#include <iostream>

#include <boost/program_options.hpp>

#include "common/GeneralException.h"
#include "consumer/MinecraftMQTT2LSL.h"

using namespace std;
namespace po = boost::program_options;

int main(int argc, char* argv[]) {
    string mqtt_address;
    int mqtt_port;

    po::options_description arguments("Program Options");
    arguments.add_options()(
        "help,h",
        "This program monitors MQTT messages from Minecraft topics "
        "and pushes them LSL upon arrival.")("mqtt_addr",
                                             po::value<string>(&mqtt_address)
                                                 ->default_value("localhost")
                                                 ->required(),
                                             "Address of the MQTT server.")(
        "mqtt_port",
        po::value<int>(&mqtt_port)->default_value(1883)->required(),
        "Address of the MQTT server.");

    try {
        MinecraftMQTT2LSL consumer = MinecraftMQTT2LSL();
        consumer.start(mqtt_address, mqtt_port);
    }
    catch (const GeneralException& ex) {
        cout << ex.what() << endl;
    }

    return 0;
}
