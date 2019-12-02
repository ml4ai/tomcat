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

  Mission Mission::from_mission_id(int mission_id) {
    string xml = Mission::getWorldSkeletonFromXML(mission_id);
    return Mission::from_XML_string(xml);
  }

  Mission Mission::fromMissionIdOrPathToXML(string missionIdOrPathToXML) {
    path p(missionIdOrPathToXML);
    if (p.extension() == ".xml") {
      return Mission::from_XML_file(missionIdOrPathToXML);
    }
    else {
      return Mission::from_mission_id(std::stoi(missionIdOrPathToXML));
    }
  }

  string Mission::getWorldSkeletonFromXML(int mission_id) {
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
                                     Mission::IdToWorldFolderMap.at(mission_id),
                                     mission_id);

    return worldSkeletonXML;
  }
} // namespace tomcat
