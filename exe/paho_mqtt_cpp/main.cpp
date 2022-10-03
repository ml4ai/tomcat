#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <mqtt/async_client.h>


/* A Paho MQTT hello world program to test Docker */
/* Further reading:  https://github.com/eclipse/paho.mqtt.c */

int main(int argc, char* argv[]) {

    std::cout << "Hello world!" << std::endl;

    /*
    char* address = "tcp://localhost:1883";

    MQTTClient handle;

    MQTTClient_connectOptions options;

    int rc = MQTTClient_connect(handle, &options);
    */


    /*
    mqtt::async_client async_client = mqtt::async_client(
        address, "paho_mqtt_async_client");
	*/

    return EXIT_SUCCESS;
}
