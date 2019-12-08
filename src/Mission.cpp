#include "Mission.h"
#include "FileHandler.h"
#include <boost/filesystem.hpp>
#include <fmt/format.h>

using namespace malmo;
using namespace std;
using boost::filesystem::path;
using fmt::format;

namespace tomcat {

  Mission Mission::from_XML_string(string xml) { return Mission(xml, true); }

  Mission Mission::from_XML_file(string missionIdOrPathToXML) {
    string xml = FileHandler::getFileContent(missionIdOrPathToXML);
    return Mission::from_XML_string(xml);
  }

  Mission Mission::from_mission_id(int missionID, unsigned int timeLimitInSeconds) {
    string xml = Mission::getWorldSkeletonFromXML(missionID, timeLimitInSeconds);
    return Mission::from_XML_string(xml);
  }

  Mission Mission::fromMissionIdOrPathToXML(string missionIdOrPathToXML, unsigned int timeLimitInSeconds) {
    path p(missionIdOrPathToXML);
    Mission newMission;
    if (p.extension() == ".xml") {
      newMission = Mission::from_XML_file(missionIdOrPathToXML);
      newMission.timeLimitInSeconds(timeLimitInSeconds);
    }
    else {
      newMission = Mission::from_mission_id(std::stoi(missionIdOrPathToXML), timeLimitInSeconds);
    }
    newMission.timeLimit = timeLimitInSeconds;
    return newMission;
  }

  string Mission::getWorldSkeletonFromXML(int missionID, unsigned int timeLimitInSeconds) {
    string worldSkeletonXML = format(R"(
    <?xml version="1.0" encoding="UTF-8"?>
    <Mission xmlns="http://ProjectMalmo.microsoft.com" 
             xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"> 
      <About>
          <Summary>Search and Rescue</Summary>
      </About>
      <ServerSection>
          <ServerInitialConditions>
            <AllowSpawning>false</AllowSpawning>
          </ServerInitialConditions>
          <ServerHandlers>
            <FileWorldGenerator 
              src="{}/data/worlds/{}" 
              forceReset="true"
            />
            <TomcatDecorator 
              mission="{}"
              timeLimitInSeconds="{}"
            />
          </ServerHandlers>
      </ServerSection>
      <AgentSection mode="Survival">
          <Name>Tomcat</Name>
          <AgentStart>
          </AgentStart>
          <AgentHandlers>
            <ContinuousMovementCommands turnSpeedDegs="840">
                <ModifierList type="deny-list">
                  <command>strafe</command>
                </ModifierList>
            </ContinuousMovementCommands>
          </AgentHandlers>
      </AgentSection>
    </Mission>)",
                                     getenv("TOMCAT"),
                                     Mission::IdToWorldFolderMap.at(missionID),
                                     missionID,
                                     timeLimitInSeconds);

    return worldSkeletonXML;
  }
} // namespace tomcat
