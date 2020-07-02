#include <iostream>
#include <csignal>
#include "WebcamSensor.h"

using namespace std;
using namespace tomcat;
void signalHandler(int signum){
    cout<<"Interrupt signal {" <<signum<<")received.\n";
    //cleanup and terminate the program
    exit(signum);
}

int main(){
    
    //register signal SIGTERM and signal handler.
    WebcamSensor camsensor;
    camsensor.initialize();
    signal(SIGTERM,signalHandler);
    while (1){
    	camsensor.get_observation();
    }
}


