#include "mqtt/Mosquitto.h"
#include <fmt/format.h>
#include <iostream>

using namespace std;

void print_message(const string& topic, const string& message) {
    cout << fmt::format("[{}] {}", topic, message) << endl;
}

int main(int argc, char* argv[]) {
    Mosquitto mosquitto = Mosquitto();

    try {
        mosquitto.connect("localhost", 1883);
        mosquitto.set_on_message_callback(&print_message);
        mosquitto.subscribe("#");
        mosquitto.loop_forever();
    }
    catch (const string& ex) {
        cout << ex << endl;
    }

    return 0;
}
