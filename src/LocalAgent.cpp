#include <exception>
#include <string>
#include <thread>
#include "LocalAgent.h"
#include <AgentHost.h>
#include <ClientPool.h>
#include <fmt/core.h>

using fmt::print;
using namespace malmo;
using namespace std;
using namespace std::this_thread;
using namespace std::chrono;

namespace tomcat {

    LocalAgent::LocalAgent() { }

    LocalAgent::~LocalAgent() { }

    void LocalAgent::setMission(string missionIdOrPathToXML, unsigned int timeLimitInSeconds,
            unsigned int width, unsigned int height) {
        //this->mission.requestVideo(width, height);
        this->missionHandler = MissionHandler();
        this->missionHandler.setMission(missionIdOrPathToXML);
        this->missionHandler.setTimeLimitInSeconds(timeLimitInSeconds);
    }

    int LocalAgent::startMission(int portNumber = 10000, bool activateWebcam = false) {
        using boost::shared_ptr;
        MissionRecordSpec missionRecord;
        WorldState worldState;
        int attempts = 0;
        bool connected = false;

        ClientPool clientPool = getClientPool(portNumber);

        print("Waiting for the mission to start...");
        do {
            try {
                this->missionHandler.startMission();
                this->host.startMission(this->missionHandler.getMissionSpec(), clientPool,
                        missionRecord,0,"");
                connected = true;
            } catch (exception &e) {
                print("Error starting mission: {}", e.what());
                attempts += 1;
                // Give up after three attempts.
                if (attempts >= 3)
                    return EXIT_FAILURE;
                else
                    // Wait a second and try again.
                    sleep_for(milliseconds(1000));
            }
        } while (!connected);

        do {
            sleep_for(milliseconds(100));
            worldState = this->host.getWorldState();
        } while (!worldState.has_mission_begun);

        if (activateWebcam) {
            this->webcamSensor.initialize();
        }

        do {
            sleep_for(milliseconds(10));
            if (activateWebcam) {
                this->webcamSensor.get_observation();
            }
            worldState = this->host.getWorldState();
        } while (worldState.is_mission_running);

        print("Mission has stopped.");

        return EXIT_SUCCESS;
    }

    ClientPool LocalAgent::getClientPool(int portNumber) const {
        ClientPool clientPool;
        clientPool.add(ClientInfo("127.0.0.1", portNumber));
        return clientPool;
    }

    void LocalAgent::sendCommand(string command) {
        this->host.sendCommand(command);
    }

} // namespace tomcat
