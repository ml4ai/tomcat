#include "MissionFactory.h"
#include "Mission.h"
#include "TomcatMission.h"
#include "XMLMission.h"
#include "FileHandler.h"

using namespace std;

namespace tomcat {

    Mission* MissionFactory::create(string missionIdOrPathToXML) {
        Mission *mission;
        string fileExtension = FileHandler::getFileExtensionFromFilename(missionIdOrPathToXML);
        if (fileExtension == "xml") {
            mission = new XMLMission(missionIdOrPathToXML);
        } else {
            stringstream missionIdAsString(missionIdOrPathToXML);
            int missionId;
            missionIdAsString >> missionId;

            mission = new TomcatMission(missionId);
        }
        
        return mission;
    }

} // namespace tomcat
