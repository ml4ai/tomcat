#include <chrono>
#include <iostream>
#include <thread>
#include <unistd.h>

#include "nlohmann/json.hpp"
#include <boost/program_options.hpp>

#include "common/GeneralException.h"
#include "data_stream/Mosquitto.h"

using namespace std;
using namespace nlohmann;
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
        Mosquitto mosquitto = Mosquitto();
        mosquitto.connect(mqtt_address, mqtt_port);

        json message;
        message["header"]["time"] = "now";
        message["header"]["type"] = "dummy";
        message["msg"]["subtype"] = "dummy_dummy";

        for (int i = 0; i < 10000; i++) {
            message["data"]["measure"] = i;
            mosquitto.publish("dummy", message.dump());
            this_thread::sleep_for(chrono::milliseconds(10));
        }
    }
    catch (const GeneralException& ex) {
        cout << ex.what() << endl;
    }

    return 0;
}
