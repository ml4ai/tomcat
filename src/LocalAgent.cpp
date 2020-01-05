#include "LocalAgent.h"
#include <AgentHost.h>
#include <ClientPool.h>
#include <exception>
#include <fmt/format.h>
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
                              unsigned int selfReportPromptTimeInSeconds,
                              unsigned int width,
                              unsigned int height,
                              bool activateVideo,
                              bool activateObsRec) {

    this->mission =
        Mission::fromMissionIdOrPathToXML(missionIdOrPathToXML,
                                          timeLimitInSeconds,
                                          selfReportPromptTimeInSeconds);
    if (activateVideo) {
      this->mission.requestVideo(width, height);
    }

    if (activateObsRec) {
      this->mission.observeRecentCommands();
      this->mission.observeHotBar();
      this->mission.observeFullInventory();
      this->mission.observeChat();
    }
  }

  int LocalAgent::startMission(int portNumber,
                               bool activateWebcam,
                               bool activateVideo,
                               bool activateMicrophone,
                               bool activateObsRec,
                               bool activateComRec,
                               bool activateRewRec,
                               int frames_per_second,
                               int64_t bit_rate,
                               string recordPath,
                               string audio_record_path) {
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
        this->host.startMission(
            this->mission, clientPool, missionRecord, 0, "");
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

    if (activateMicrophone) {
      this->microphone.set_time_limit_in_seconds(
          this->mission.getTimeLimitInSeconds());
      this->microphone.initialize();
    }

    if (activateWebcam) {
      this->webcamSensor = new WebcamSensor();
      this->webcamSensor->initialize();
    }

    do {
      sleep_for(milliseconds(10));
      if (activateWebcam) {
        this->webcamSensor->get_observation();
      }
      worldState = this->host.getWorldState();
    } while (worldState.is_mission_running);

    if (activateMicrophone) {
      this->microphone.set_output_filename(audio_record_path);
      this->microphone.finalize();
    }

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
