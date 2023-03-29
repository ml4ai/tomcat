#include <iostream>
#include <unistd.h>
#include <chrono>
#include <thread>

#include "nlohmann/json.hpp"

#include "data_stream/Mosquitto.h"
#include "common/GeneralException.h"

using namespace std;
using namespace nlohmann;

int main(int argc, char* argv[]) {
    try {
        Mosquitto mosquitto = Mosquitto();
        mosquitto.connect("localhost", 1883);

        json message;
        message["header"]["time"] = "now";
        message["header"]["type"] = "random";
        message["msg"]["subtype"] = "event";

        for (int i =0; i < 10000; i++) {
            message["data"]["measure"] = i;
            mosquitto.publish("random", message.dump());
            this_thread::sleep_for(chrono::milliseconds(10));
        }
    }
    catch (const GeneralException& ex) {
        cout << ex.what() << endl;
    }

    return 0;
}
