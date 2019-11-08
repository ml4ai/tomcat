// Local:
#include "XMLMission.h"
#include "FileHandler.h"

using namespace malmo;
using namespace std;

namespace tomcat {

  XMLMission::XMLMission(const string& missionIdOrPathToXML) {
    this->missionSpec =
        MissionSpec(FileHandler::getFileContent(missionIdOrPathToXML), true);
  }

  XMLMission::~XMLMission() {}

  void XMLMission::buildWorld() {}

  string XMLMission::getWorldSkeletonFromXML() { return ""; }

} // namespace tomcat
