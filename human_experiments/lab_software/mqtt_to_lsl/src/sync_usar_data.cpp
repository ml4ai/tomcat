#include <iostream>

#include "consumer/MinecraftMQTT2LSL.h"
#include "common/GeneralException.h"

using namespace std;

int main(int argc, char* argv[]) {
    try {
        MinecraftMQTT2LSL consumer = MinecraftMQTT2LSL();
        consumer.start("localhost", 1883);
    }
    catch (const GeneralException& ex) {
        cout << ex.what() << endl;
    }

    return 0;
}
