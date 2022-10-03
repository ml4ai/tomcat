#include <stdio.h>
#include <stdlib.h>
/*#include <mqtt/async_client.h>a */

/* A Paho MQTT hello world program to test Docker */

int main(int argc, char* argv[]) {

    printf("Hello world!\n");

    char* address = "tcp://localhost:1883";

    /*
    mqtt::async_client async_client = mqtt::async_client(
        address, "paho_mqtt_async_client");
	*/

    return EXIT_SUCCESS;
}
