#include <stdlib.h>
#include <string.h>
#include "MQTTAsync.h"
 
#if !defined(_WIN32)
#include <unistd.h>
#else
#include <windows.h>
#endif
 
#if defined(_WRS_KERNEL)
#include <OsWrapper.h>
#endif
 
/* Run on host machine, connect to host machine mosquitto broker */
/*#define ADDRESS     "tcp://localhost:1883"*/ 

/* Run in Docker container, connect to Testbed mqtt continer */
#define ADDRESS     "tcp://mosquitto:1883"
#define CLIENTID    "ExampleClientSub"
#define TOPIC       "docker"
#define PAYLOAD     "Hello World!"
#define QOS         1
#define TIMEOUT     10000L
 
int disc_finished = 0;
int subscribed = 0;
int finished = 0;
 
void connlost(void *context, char *cause)
{
        MQTTAsync client = (MQTTAsync)context;
        MQTTAsync_connectOptions conn_opts = MQTTAsync_connectOptions_initializer;
        int rc;
 
        printf("\nConnection lost\n");
        if (cause)
                printf("     cause: %s\n", cause);
 
        printf("Reconnecting\n");
        fflush(stdout);
        conn_opts.keepAliveInterval = 20;
        conn_opts.cleansession = 1;
        if ((rc = MQTTAsync_connect(client, &conn_opts)) != MQTTASYNC_SUCCESS)
        {
                printf("Failed to start connect, return code %d\n", rc);
                fflush(stdout);
                finished = 1;
        }
}
 
 
int msgarrvd(void *context, char *topicName, int topicLen, MQTTAsync_message *message)
{
    printf("Message arrived\n");
    printf("     topic: %s\n", topicName);
    printf("   message: %.*s\n", message->payloadlen, (char*)message->payload);
    fflush(stdout);
    MQTTAsync_freeMessage(&message);
    MQTTAsync_free(topicName);
    return 1;
}
 
void onDisconnectFailure(void* context, MQTTAsync_failureData* response)
{
        printf("Disconnect failed, rc %d\n", response->code);
        fflush(stdout);
        disc_finished = 1;
}
 
void onDisconnect(void* context, MQTTAsync_successData* response)
{
        printf("Successful disconnection\n");
        fflush(stdout);
        disc_finished = 1;
}
 
void onSubscribe(void* context, MQTTAsync_successData* response)
{
        printf("Subscribe succeeded\n");
        fflush(stdout);
        subscribed = 1;
}
 
void onSubscribeFailure(void* context, MQTTAsync_failureData* response)
{
        printf("Subscribe failed, rc %d\n", response->code);
        fflush(stdout);
        finished = 1;
}
 
 
void onConnectFailure(void* context, MQTTAsync_failureData* response)
{
        printf("Connect failed, rc %d\n", response->code);
        fflush(stdout);
        finished = 1;
}
 
 
void onConnect(void* context, MQTTAsync_successData* response)
{
        MQTTAsync client = (MQTTAsync)context;
        MQTTAsync_responseOptions opts = MQTTAsync_responseOptions_initializer;
        int rc;
 
        printf("Successful connection\n");
 
        printf("Subscribing to topic %s\nfor client %s using QoS%d\n\n"
           "Press Q<Enter> to quit\n\n", TOPIC, CLIENTID, QOS);
        fflush(stdout);
        opts.onSuccess = onSubscribe;
        opts.onFailure = onSubscribeFailure;
        opts.context = client;
        if ((rc = MQTTAsync_subscribe(client, TOPIC, QOS, &opts)) != MQTTASYNC_SUCCESS)
        {
                printf("Failed to start subscribe, return code %d\n", rc);
                fflush(stdout);
                finished = 1;
        }
}
 
 
int main(int argc, char* argv[])
{
	printf("main called\n");
	fflush(stdout);
        MQTTAsync client;
        MQTTAsync_connectOptions conn_opts = MQTTAsync_connectOptions_initializer;
        MQTTAsync_disconnectOptions disc_opts = MQTTAsync_disconnectOptions_initializer;
        int rc;
        int ch;
 
        if ((rc = MQTTAsync_create(&client, ADDRESS, CLIENTID, MQTTCLIENT_PERSISTENCE_NONE, NULL))
                        != MQTTASYNC_SUCCESS)
        {
                printf("Failed to create client, return code %d\n", rc);
	        fflush(stdout);
                rc = EXIT_FAILURE;
                goto exit;
        }
	printf("MQTTAsync_create called\n");
	fflush(stdout);
 
        if ((rc = MQTTAsync_setCallbacks(client, client, connlost, msgarrvd, NULL)) != MQTTASYNC_SUCCESS)
        {
                printf("Failed to set callbacks, return code %d\n", rc);
	        fflush(stdout);
                rc = EXIT_FAILURE;
                goto destroy_exit;
        }
	printf("MQTTAsync_setCallbacks called\n");
	fflush(stdout);
 
        conn_opts.keepAliveInterval = 20;
        conn_opts.cleansession = 1;
        conn_opts.onSuccess = onConnect;
        conn_opts.onFailure = onConnectFailure;
        conn_opts.context = client;
        if ((rc = MQTTAsync_connect(client, &conn_opts)) != MQTTASYNC_SUCCESS)
        {
                printf("Failed to start connect, return code %d\n", rc);
	        fflush(stdout);
                rc = EXIT_FAILURE;
                goto destroy_exit;
        }
	printf("subscribed = %d\n", subscribed);
	printf("finished = %d\n", finished);
	printf("MQTTAsync test passed, rc = %d\n", rc);
	fflush(stdout);
 
	int counter = 0;

        while (!subscribed && !finished) {
            printf("test loop iteration %d\n", counter);
            fflush(stdout);
	    counter += 1;
                #if defined(_WIN32)
                        Sleep(100);
                #else
                        usleep(10000L);
                #endif
	}
	printf("subscribed = %d\n", subscribed);
	printf("subscribed = %d\n", subscribed);
	printf("finished = %d\n", finished);
	printf("finished = %d\n", finished);
        printf("test loop completed\n");
        fflush(stdout);
 
        if (finished)
                goto exit;
 
	printf("Entering main loop\n");
	fflush(stdout);
        do
        {
                ch = getchar();
        } while (ch!='Q' && ch != 'q');
 
        disc_opts.onSuccess = onDisconnect;
        disc_opts.onFailure = onDisconnectFailure;
        if ((rc = MQTTAsync_disconnect(client, &disc_opts)) != MQTTASYNC_SUCCESS)
        {
                printf("Failed to start disconnect, return code %d\n", rc);
	        fflush(stdout);
                rc = EXIT_FAILURE;
                goto destroy_exit;
        }
        while (!disc_finished)
        {
                #if defined(_WIN32)
                        Sleep(100);
                #else
                        usleep(10000L);
                #endif
        }
 
destroy_exit:
        MQTTAsync_destroy(&client);
exit:
        return rc;
}

