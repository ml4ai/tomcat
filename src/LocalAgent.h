#pragma once

#include "WebcamSensor.h"
#include <AgentHost.h>
#include <fmt/core.h>
#include <string>

/**
 * The LocalAgent class is a template for ToMCAT agents that accompany human
 * players.
 */
class LocalAgent {

  public:
  /** Default constructor.  */
  LocalAgent() {}

  /**
   * Set the agent's mission from an XML file.
   * @param mission_xml_filepath The path to the XML file containing the mission
   * specification.
   * @param time_limit The time limit for the mission.
   */
  void set_mission(std::string mission_xml_filepath, unsigned int time_limit);

  int startMission(int);
  void sendCommand(std::string);

  WebcamSensor webcam_sensor;
  malmo::MissionSpec mission;
  malmo::AgentHost host;
};
