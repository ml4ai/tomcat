#include "Mosquitto.h"

#include <iostream>
#include <unistd.h>

#define MAX_NUM_RECONNECTIONS 5

using namespace std;

//----------------------------------------------------------------------
// Constructors & Destructor
//----------------------------------------------------------------------
Mosquitto::Mosquitto() {
    mosquitto_lib_init();
    this->running = false;
    this->mqtt_client = mosquitto_new(NULL, true, this);
    if (this->mqtt_client == NULL)
        throw "Error creating mosquitto instance";

    mosquitto_connect_callback_set(this->mqtt_client,
                                   &mosquitto_callback_on_connect);
    mosquitto_message_callback_set(this->mqtt_client,
                                   &mosquitto_callback_on_message);
}

Mosquitto::~Mosquitto() {
    mosquitto_destroy(this->mqtt_client);
    mosquitto_lib_cleanup();
}

//----------------------------------------------------------------------
// Copy & Move constructors/assignments
//----------------------------------------------------------------------
Mosquitto::Mosquitto(const Mosquitto& mosquitto) {
    this->copy_wrapper(mosquitto);
}

Mosquitto& Mosquitto::operator=(const Mosquitto& mosquitto) {
    this->copy_wrapper(mosquitto);
    return *this;
}

//----------------------------------------------------------------------
// Static functions
//----------------------------------------------------------------------
void Mosquitto::mosquitto_callback_on_connect(struct mosquitto* mqtt_client,
                                              void* wrapper_instance,
                                              int error_code) {
    Mosquitto* mosquitto = (Mosquitto*)wrapper_instance;

    switch (error_code) {
    case 0:
        // Connected
        mosquitto->on_connected();
        break;
    case 1:
        mosquitto->on_error(
            "Connection refused (unacceptable protocol version)");
        break;
    case 2:
        mosquitto->on_error("Connection refused (identifier rejected)");
        break;
    case 3:
        mosquitto->on_error("Connection refused (broker unavailable)");
        break;
    case 5:
        mosquitto->on_error("Connection refused (Username/Password wrong) ");
        break;
    default:
        stringstream ss;
        ss << "Connection failed (Error code " << error_code << ")";
        string msg = ss.str();
        mosquitto->on_error(msg.c_str());
        break;
    }
}

void Mosquitto::mosquitto_callback_on_message(
    struct mosquitto* mqtt_client,
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
    mosquitto->last_updated_time = chrono::steady_clock::now();
    mosquitto->on_message(topic, ss.str());
}

//----------------------------------------------------------------------
// Virtual functions
//----------------------------------------------------------------------
void Mosquitto::on_connected() { cout << "Connected!" << endl; }

void Mosquitto::on_error(const string& error_message) {}

void Mosquitto::on_message(const string& topic,
                           const string& message) {}

void Mosquitto::on_time_out() {}

//----------------------------------------------------------------------
// Member functions
//----------------------------------------------------------------------
void Mosquitto::copy_wrapper(const Mosquitto& mosquitto) {
    this->mqtt_client = mosquitto.mqtt_client;
    this->running = mosquitto.running;
}

void Mosquitto::connect(const string& address, int port, int alive_delay) {
    int error_code = mosquitto_connect(
        this->mqtt_client, address.c_str(), port, alive_delay);
    if (error_code != MOSQ_ERR_SUCCESS) {
        stringstream ss;
        ss << "It was not possible to establish connection with the "
              "message broker at "
           << address << ":" << port;
        this->on_error(ss.str());
    }
    this->running = true;
}

void Mosquitto::subscribe(const string& topic) {
    const int qos = 0;
    int error_code =
        mosquitto_subscribe(this->mqtt_client, NULL, topic.c_str(), qos);
    if (error_code != MOSQ_ERR_SUCCESS) {
        stringstream ss;
        ss << "It was not possible to subscribe to the topic " << topic;
        this->on_error(ss.str());
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
        stringstream ss;
        ss << "It was not possible to publish in the topic " << topic;
        this->on_error(ss.str());
    }
}

void Mosquitto::loop(const bool try_reconnect) {
    int error_counter = 0;
    int max_packets = 10;
    this->last_updated_time = chrono::steady_clock::now();

    do {
        int error_code = mosquitto_loop(this->mqtt_client, -1, 10);
        if (this->time_out()) {
            this->on_time_out();
            this->running = false;
        }

        // A derived class can change this variable so it can stop running for
        // extraneous reasons, not only because of timeout.
        if (!this->running) {
            return;
        }

        if (error_code != MOSQ_ERR_SUCCESS) {
            sleep(10);
            error_code = mosquitto_reconnect(this->mqtt_client);

            if (error_code == MOSQ_ERR_SUCCESS) {
                // Reconnect successfull
                continue;
            }
            else {
                // Reconnect failed. Increase counter
                error_counter++;

                if (error_counter > MAX_NUM_RECONNECTIONS) {
                    this->on_error(
                        "Maximum number of reconnection attempts was reached.");
                    this->running = false;
                }
            }
        }
        else {
            error_counter = 0;
        }
    } while (this->running && try_reconnect);
}

bool Mosquitto::time_out() const {
    Time current_time = chrono::steady_clock::now();
    long duration = chrono::duration_cast<chrono::seconds>(
                        current_time - this->last_updated_time)
                        .count();
    return duration > this->max_seconds_without_messages;
}

void Mosquitto::close() {
    this->running = false;
    mosquitto_disconnect(this->mqtt_client);
}

void Mosquitto::set_max_seconds_without_messages(
    long max_seconds_without_messages) {
    this->max_seconds_without_messages = max_seconds_without_messages;
}
