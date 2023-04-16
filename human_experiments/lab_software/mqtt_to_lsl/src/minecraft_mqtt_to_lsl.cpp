#include <iostream>
#include <exception>

#include <boost/program_options.hpp>

#include "consumer/MinecraftMQTT2LSL.h"
#include "common/SignalHandler.h"

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

    po::variables_map vm;
    po::store(po::parse_command_line(argc, argv, arguments), vm);
    po::notify(vm);
    if (vm.count("help")) {
        cout << arguments << "\n";
        return 1;
    }

    try {
        MinecraftMQTT2LSL minecraft_consumer = MinecraftMQTT2LSL();

        // Signal handler in case the program is interrupted.
        watch_for_signal();
        minecraft_consumer.start(mqtt_address, mqtt_port, &quit);
    }
    catch (const std::exception& ex) {
        cerr << "[ERROR] Program crashed." << endl;
        cerr << ex.what() << endl;
    }

    return 0;
}
