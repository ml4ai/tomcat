#pragma once

#include "Mission.h"
#include "Microphone.h"
#include "WebcamSensor.h"
#include <AgentHost.h>
#include <fmt/format.h>
#include <string>

namespace tomcat {

  /**
   * The LocalAgent class is a template for ToMCAT agents that accompany human
   * players.
   */
  class LocalAgent {

  public:
    /**
     * Constructor
     * */
    LocalAgent();

    /**
     * Destructor
     * */
    ~LocalAgent();

    /**
     * Sets the agent's mission from an XML file.
     * @param mission_xml_filepath The path to the XML file containing the
     * mission specification.
     * @param time_limit The time limit for the mission.
     * @param activateVideo Requests video when set to True
     * @param activateAudio Activates audio recording
     */
    void setMission(std::string missionIdOrPathToXML,
                    unsigned int width = 640,
                    unsigned int height = 480,
                    bool activateVideo = false,
                    bool activateObsRec = false);

    /** Set the time limit in seconds for the mission */
    void setMissionTimeLimit(unsigned int timeLimitInSeconds = 20);
    /**
     * Starts a mission
     * @param portNumber - Port number to connect with the Minecraft mod
     * @param activateWebcam - Option to activate the webcam to perform face
     * tracking
     * @return
     */
    int startMission(int portNumber = 10000,
                     bool activateWebcam = false,
                     bool activateVideo = false,
                     bool activateMicrophone = false,
                     bool activateObsRec = false,
                     bool activateComRec = false,
                     bool activateRewRec = false,
                     int frames_per_second = 20,
                     int64_t bit_rate = 400000,
                     std::string recordPath = "./saved_data.tgz");

    /**
     * Sends command to the local agent
     * @param command - Command to be executed
     */
    void sendCommand(std::string command);

  private:
    /**
     * Retrieves the client pool for connecting with the Minecraft mod
     * @param portNumber - Port number to connect with the Minecraft mod
     */
    malmo::ClientPool getClientPool(int portNumber) const;

    WebcamSensor* webcamSensor;
    malmo::AgentHost host;
    Mission mission;
    Microphone microphone;
  };

} // namespace tomcat
