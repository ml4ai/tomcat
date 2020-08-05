/* This is an example program to demonstrate the usage of the Mosquitto client
 * library (https://mosquitto.org/man/libmosquitto-3.html). It subscribes to
 * all the topics under 'irc' in a topic hierarchy (note the usage of the
 * wildcard character '#'). For every message received it prints the topic, the
 * message, and the length of the message.
 *
 * This example has been adapted from the 'callback.c' in the official
 * mosquitto source tree
 * (https://github.com/eclipse/mosquitto/blob/master/examples/subscribe_simple/callback.c)
 * by Adarsh Pyarelal (adarsh@arizona.edu).
 *
 * For the full API documentation of libmosquitto, see
 * https://mosquitto.org/api/files/mosquitto-h.html.
 *
 * Usage: ./libmosquitto_example
 */

#include <mosquitto.h>
#include <stdio.h>
#include <stdlib.h>

/* on_message is a 'callback' function - it will be called everytime there is a
 * message published on the topic named 'example_topic' (see call to
 * mosquitto_subscribe_callback in the 'main' function below). */
int on_message(struct mosquitto* mosq,
               void* userdata,
               const struct mosquitto_message* msg) {
    printf(
        "%s %s (%d)\n", msg->topic, (const char*)msg->payload, msg->payloadlen);
    return 0;
}

int main(int argc, char* argv[]) {
    /* We declare an integer variable to hold the return code for our C API
     * calls. */
    int rc;

    // mosquitto_lib_init must be called before any other mosquitto function.
    mosquitto_lib_init();

    /* We then associate the on_message callback function to process messages
     * received on the 'irc' topic and all topics under it in the topic
     * hierarchy.
     *
     * mosquitto_subscribe_callback is a helper function to make implementing
     * callbacks easier.
     * See
     * https://mosquitto.org/api/files/mosquitto-h.html#mosquitto_subscribe_callback
     * for descriptions of the arguments to this function.
     * */
    rc = mosquitto_subscribe_callback(on_message,
                                      NULL,
                                      "irc/#",
                                      0,
                                      "localhost",
                                      1883,
                                      NULL,
                                      60,
                                      true,
                                      NULL,
                                      NULL,
                                      NULL,
                                      NULL);

    if (rc) {
        printf("Error: %s\n", mosquitto_strerror(rc));
    }

    // mosquitto_lib_cleanup must be called to free resources associated with
    // the library.
    mosquitto_lib_cleanup();

    return rc;
}
