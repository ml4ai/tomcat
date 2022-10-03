#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <MQTTClient.h>

/* A Paho MQTT hello world program to test Docker */
/* Further reading:  https://github.com/eclipse/paho.mqtt.c */

int main(int argc, char* argv[]) {

    printf("Hello world!\n");

    char* address = "tcp://localhost:1883";

    MQTTClient handle;

    MQTTClient_connectOptions options;

    int rc = MQTTClient_connect(handle, &options);


    /*
    mqtt::async_client async_client = mqtt::async_client(
        address, "paho_mqtt_async_client");
	*/

    return EXIT_SUCCESS;
}
