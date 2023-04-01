#include "Mosquitto.h"

#include <iostream>
#include <sstream>
#include <stdexcept>
#include <thread>

#include "fmt/format.h"

#include "common/GeneralException.h"

#define ONE_SECOND 1000   // in milliseconds
#define FIVE_SECONDS 5000 // in milliseconds

using namespace std;

//----------------------------------------------------------------------
// Constructors & Destructor
//----------------------------------------------------------------------
Mosquitto::Mosquitto() {
    mosquitto_lib_init();
    this->mqtt_client = mosquitto_new(nullptr, true, this);
    if (this->mqtt_client == nullptr)
        throw GeneralException("Error creating mosquitto instance");

    mosquitto_message_callback_set(this->mqtt_client, &on_message_callback);
}

Mosquitto::~Mosquitto() {
    mosquitto_destroy(this->mqtt_client);
    mosquitto_lib_cleanup();
}

//----------------------------------------------------------------------
// Defining callbacks
//----------------------------------------------------------------------
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

    auto* mosquitto = (Mosquitto*)wrapper_instance;
    mosquitto->on_message_external_callback(topic, ss.str());
}

void Mosquitto::set_on_message_callback(
    function<void(const string&, const string&)> callback_fn) {
    this->on_message_external_callback = move(callback_fn);
}

//----------------------------------------------------------------------
// Member functions
//----------------------------------------------------------------------

void Mosquitto::connect(const string& address, int port, int trials) {

    while (trials > 0) {
        cout << "Trying to connect to " << address << ":" << port << "..."
             << endl;
        int error_code = mosquitto_connect(
            this->mqtt_client, address.c_str(), port, ONE_SECOND);

        try {
            switch (error_code) {
            case MOSQ_ERR_SUCCESS:
                cout << "Connection established!" << endl;
                trials = 0; // no need to keep trying to connect.
                break;
            case MOSQ_ERR_PROTOCOL:
                throw GeneralException(
                    "Connection refused (unacceptable protocol version)");
            case MOSQ_ERR_CONN_REFUSED:
                throw GeneralException(
                    "Connection refused (identifier rejected)");
            case MOSQ_ERR_NO_CONN:
                throw GeneralException(
                    "Connection refused (broker unavailable)");
            default:
                throw GeneralException(fmt::format(
                    "Connection failed (Error code {})", error_code));
            }
        }
        catch (const GeneralException& ex) {
            trials--;
            if (trials > 0) {
                cout << fmt::format("Fail to connect. {}", ex.what()) << endl;
                cout << fmt::format("We will try once more in {} seconds.",
                                    FIVE_SECONDS / 1000)
                     << endl;
                this_thread::sleep_for(chrono::milliseconds(FIVE_SECONDS));
            }
            else {
                throw ex;
            }
        }
    }
}

void Mosquitto::subscribe(const string& topic) {
    const int qos = 0;
    int error_code =
        mosquitto_subscribe(this->mqtt_client, nullptr, topic.c_str(), qos);

    if (error_code != MOSQ_ERR_SUCCESS) {
        throw GeneralException(fmt::format(
            "It was not possible to subscribe to the topic {}. Error code {}.",
            topic,
            error_code));
    }
}

void Mosquitto::publish(const string& topic, const string& message) {
    const char* payload = message.c_str();
    const int len = (int)message.size();

    const int qos = 0;
    int error_code = mosquitto_publish(this->mqtt_client,
                                       nullptr,
                                       topic.c_str(),
                                       len,
                                       (const void*)payload,
                                       qos,
                                       false);

    if (error_code != MOSQ_ERR_SUCCESS) {
        throw GeneralException(fmt::format(
            "It was not possible to publish in the topic {}. Error code {}.",
            topic,
            error_code));
    }
}

void Mosquitto::start() {
    // Ignore if it's already running
    if (this->running) {
        return;
    }

    cout << "Watching MQTT messages...." << endl;

    this->running = true;
    this->loop_thread = thread([this] { this->loop(); });
}

void Mosquitto::stop() {
    // Ignore if it's not running
    if (!this->running) {
        return;
    }

    cout << "Stopping MQTT watcher..." << endl;

    this->running = false;

    // Wait for the thread to finish whatever it is doing.
    this->loop_thread.join();

    mosquitto_disconnect(this->mqtt_client);
}

void Mosquitto::loop() {
    while(this->running) {
        int error_code = mosquitto_loop(this->mqtt_client, -1, 1);

        if (error_code != MOSQ_ERR_SUCCESS) {
            throw GeneralException(
                fmt::format("Fail to loop. Error code {}.", error_code));
        }
    }
}
