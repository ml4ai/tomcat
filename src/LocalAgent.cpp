#include "LocalAgent.h"
#include <AgentHost.h>
#include <ClientPool.h>
#include <exception>
#include <fmt/core.h>
#include <string>
#include <thread>

using fmt::print;
using namespace malmo;
using namespace std;
using namespace std::this_thread;
using namespace std::chrono;

namespace tomcat {

  LocalAgent::LocalAgent() {}
  LocalAgent::~LocalAgent() {}

  void LocalAgent::setMission(string missionIdOrPathToXML,
                              unsigned int timeLimitInSeconds,
                              unsigned int width,
                              unsigned int height,
                              bool activateVideo,
                              bool activateObsRec) {
    this->missionHandler = MissionHandler();
    this->missionHandler.setMission(missionIdOrPathToXML);
    this->missionHandler.setTimeLimitInSeconds(timeLimitInSeconds);

    if (activateVideo) {
      this->missionHandler.requestVideo(width, height);
    }

    if (activateObsRec) {
      this->missionHandler.observeRecentCommands();
      this->missionHandler.observeHotBar();
      this->missionHandler.observeFullInventory();
      this->missionHandler.observeChat();
    }
  }

  int LocalAgent::startMission(int portNumber,
                               bool activateWebcam,
                               bool activateVideo,
                               bool activateObsRec,
                               bool activateComRec,
                               bool activateRewRec,
                               int frames_per_second,
                               int64_t bit_rate,
                               std::string recordPath) {
    using boost::shared_ptr;
    MissionRecordSpec missionRecord(recordPath);

    if (activateVideo) {
      missionRecord.recordMP4(frames_per_second, bit_rate);
    }

    if (activateObsRec) {
      missionRecord.recordObservations();
    }

    if (activateComRec) {
      missionRecord.recordCommands();
    }

    if (activateRewRec) {
      missionRecord.recordRewards();
    }

    ClientPool clientPool = getClientPool(portNumber);

    print("Waiting for the mission to start...");
    int attempts = 0;
    bool connected = false;
    do {
      try {
        this->missionHandler.startMission();
        this->host.startMission(this->missionHandler.getMissionSpec(),
                                clientPool,
                                missionRecord,
                                0,
                                "");
        connected = true;
      }
      catch (exception& e) {
        print("Error starting mission: {}", e.what());
        attempts += 1;
        // Give up after three attempts.
        if (attempts >= 3) {
          return EXIT_FAILURE;
        }
        else {
          // Wait a second and try again.
          sleep_for(milliseconds(1000));
        }
      }
    } while (!connected);

    WorldState worldState;
    do {
      sleep_for(milliseconds(100));
      worldState = this->host.getWorldState();
    } while (!worldState.has_mission_begun);

    this->microphone.set_time_limit_in_seconds(this->missionHandler.getTimeLimitInSeconds());
    this->microphone.initialize();

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

    this->microphone.finalize();

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
