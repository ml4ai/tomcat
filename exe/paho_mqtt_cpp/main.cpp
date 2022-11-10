#include <iostream>
#include <cstdlib>
#include <string>
#include <thread>
#include <atomic>
#include <chrono>
#include <cstring>
#include "mqtt/async_client.h"

#define ADDRESS     "tcp://mosquitto:1883"
#define CLIENTID    "ExampleClientSub"


int main(int argc, char* argv[])
{
    std::cout << "Hello world!" << std::endl;

    // demonstrate that library objects are accessible
    mqtt::async_client ac(ADDRESS, CLIENTID, nullptr);

}
