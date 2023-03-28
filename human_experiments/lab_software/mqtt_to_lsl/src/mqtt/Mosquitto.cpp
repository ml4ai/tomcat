#include "Mosquitto.h"

#include "fmt/format.h"
#include <iostream>
#include <thread>

#define ONE_SECOND 1000 // in milliseconds

using namespace std;

//----------------------------------------------------------------------
// Constructors & Destructor
//----------------------------------------------------------------------
Mosquitto::Mosquitto() {
    mosquitto_lib_init();
    this->mqtt_client = mosquitto_new(NULL, true, this);
    if (this->mqtt_client == NULL)
        throw "Error creating mosquitto instance";

    mosquitto_connect_callback_set(this->mqtt_client, &on_connect_callback);
    mosquitto_message_callback_set(this->mqtt_client, &on_message_callback);
}

Mosquitto::~Mosquitto() {
    mosquitto_destroy(this->mqtt_client);
    mosquitto_lib_cleanup();
}

//----------------------------------------------------------------------
// Defining callbacks
//----------------------------------------------------------------------
void Mosquitto::on_connect_callback(struct mosquitto* mqtt_client,
                                    void* wrapper_instance,
                                    int error_code) {

    switch (error_code) {
    case 0:
        break;
    case 1:
        throw "Connection refused (unacceptable protocol version)";
    case 2:
        throw "Connection refused (identifier rejected)";
    case 3:
        throw "Connection refused (broker unavailable)";
    case 5:
        throw "Connection refused (Username/Password wrong)";
    default:
        throw fmt::format("Connection failed (Error code {})", error_code);
    }
}

void Mosquitto::on_message_callback(struct mosquitto* mqtt_client,
                                    void* wrapper_instance,
                                    const struct mosquitto_message* message) {

    string topic(message->topic);

    // Build message
    stringstream ss;
    const int len = message->payloadlen;
    char* payload = (char*)message->payload;
    for (int i = 0; i < len; i++) {
        ss << (char)(payload[i]);
    }

    Mosquitto* mosquitto = (Mosquitto*)wrapper_instance;
    mosquitto->on_message_external_callback(topic, ss.str());
}

void Mosquitto::set_on_message_callback(void (*callback_fn)(const string&,
                                                            const string&)) {
    this->on_message_external_callback = callback_fn;
}

//----------------------------------------------------------------------
// Member functions
//----------------------------------------------------------------------

void Mosquitto::connect(const string& address,
                        int port,
                        int trials) {

    while (trials > 0) {
        cout << "Trying to connect to " << address << ":" << port << "..."
             << endl;
        this_thread::sleep_for(chrono::milliseconds(ONE_SECOND));
        try {
            // Our callback will throw an exception in case of fail to connect.
            mosquitto_connect(
                this->mqtt_client, address.c_str(), port, ONE_SECOND);
            cout << "Connection established!" << endl;
            trials = 0;
        }
        catch (const std::string& ex) {
            cout << ex << endl;
            trials--;
            if (trials > 0) {
                cout << "We will retry once more." << endl;
                this_thread::sleep_for(
                    chrono::milliseconds(ONE_SECOND));
            }
        }
    }
}

void Mosquitto::subscribe(const string& topic) {
    const int qos = 0;
    int error_code =
        mosquitto_subscribe(this->mqtt_client, NULL, topic.c_str(), qos);

    if (error_code != MOSQ_ERR_SUCCESS) {
        throw fmt::format(
            "It was not possible to subscribe to the topic {}. Error code {}.",
            topic,
            error_code);
    }
}

void Mosquitto::publish(const string& topic, const string& message) {
    const char* payload = message.c_str();
    const int len = (int)message.size();

    const int qos = 0;
    int error_code = mosquitto_publish(this->mqtt_client,
                                       NULL,
                                       topic.c_str(),
                                       len,
                                       (const void*)payload,
                                       qos,
                                       false);

    if (error_code != MOSQ_ERR_SUCCESS) {
        throw fmt::format(
            "It was not possible to publish in the topic {}. Error code {}.",
            topic,
            error_code);
    }
}

void Mosquitto::loop_forever() {
    int error_code = mosquitto_loop_forever(this->mqtt_client, -1, 1);

    if (error_code != MOSQ_ERR_SUCCESS) {
        throw fmt::format("Fail to loop forever. Error code {}.", error_code);
    }
}

void Mosquitto::stop() {
    mosquitto_disconnect(this->mqtt_client);
}
