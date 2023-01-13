#include "publish.h"

#include <iostream>
#include <cstring>
#include <mosquitto.h>

myMosq::myMosq(const char *_id, const char *_topic, const char *_host, int _port)
{
    mosquitto_lib_init();        // Mandatory initialization for mosquitto library

    this->keepalive = 60;    // Basic configuration setup for myMosq class
    this->id = _id;
    this->port = _port;
    this->host = _host;
    this->topic = _topic;

    this->mosq = mosquitto_new(_id, true, this);
    if(this->mosq == NULL) throw "Error creating mosquitto instance";

    mosquitto_connect_async(
        this->mosq,
        this->host, // non blocking connection to broker request
        this->port,
        this->keepalive);

    mosquitto_loop_start(this->mosq); // Start thread managing connection / publish / subscribe
};

myMosq::~myMosq() {
    mosquitto_loop_stop(this->mosq, true); // Kill the thread
    mosquitto_lib_cleanup(); // Mosquitto library cleanup
}

bool myMosq::send_message(const  char * _message)
{
// Send message - depending on QoS, mosquitto lib managed re-submission this the thread
//
// * NULL : Message Id (int *) this allow to latter get status of each message
// * topic : topic to be used
// * lenght of the message
// * message
// * qos (0,1,2)
// * retain (boolean) - indicates if message is retained on broker or not
// Should return MOSQ_ERR_SUCCESS
    int ret = mosquitto_publish(this->mosq, nullptr, this->topic, strlen(_message),_message,1,false);
    return ( ret == MOSQ_ERR_SUCCESS );
}

void myMosq::on_disconnect(int rc) {
    std::cout << ">> myMosq - disconnection(" << rc << ")" << std::endl;
}

void myMosq::on_connect(int rc)
{
    if ( rc == 0 ) {
        std::cout << ">> myMosq - connected with server" << std::endl;
    }
    else {
        std::cout << ">> myMosq - Impossible to connect with server(" << rc << ")" << std::endl;
    }
}

void myMosq::on_publish(int mid)
{
    std::cout << ">> myMosq - Message (" << mid << ") succeed to be published " << std::endl;
}

