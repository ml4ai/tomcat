#include "MissionFactory.h"
#include "Mission.h"
#include "SARMission.h"
#include "XMLMission.h"
#include "FileHandler.h"

using namespace std;

namespace tomcat {

    enum missionID { searchAndRescue = 1, itemCrafting = 2, roomEscape = 3};

    Mission* MissionFactory::create(string missionIdOrPathToXML) {
        Mission *mission;
        string fileExtension = FileHandler::getFileExtensionFromFilename(missionIdOrPathToXML);
        if (fileExtension == "xml") {
            mission = new XMLMission(missionIdOrPathToXML);
        } else {
            stringstream mission_id_as_string(missionIdOrPathToXML);
            int mission_id;
            mission_id_as_string >> mission_id;

            switch (mission_id) {
                case missionID::searchAndRescue:
                    mission = new SARMission();
                    break;

                case missionID::itemCrafting:
                    mission = new SARMission();
                    break;

                case missionID::roomEscape:
                    mission = new SARMission();
                    break;
            }
        }
        
        return mission;
    }

} // namespace tomcat
