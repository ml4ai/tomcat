#include "MissionFactory.h"
#include "FileHandler.h"
#include "Mission.h"
#include "TomcatMission.h"
#include "XMLMission.h"

using namespace std;

namespace tomcat {

  Mission* MissionFactory::create(string missionIdOrPathToXML) {
    Mission* mission;
    string fileExtension =
        FileHandler::getFileExtensionFromFilename(missionIdOrPathToXML);
    if (fileExtension == "xml") {
      mission = new XMLMission(missionIdOrPathToXML);
    }
    else {
      stringstream missionIdAsString(missionIdOrPathToXML);
      int missionId;
      missionIdAsString >> missionId;

      mission = new TomcatMission(missionId);
    }

    return mission;
  }

} // namespace tomcat
