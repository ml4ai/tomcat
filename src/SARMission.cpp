#include "SARMission.h"
#include "FileHandler.h"

using namespace malmo;
using namespace std;

namespace tomcat {

    const static int HEIGHT_OF_GROUND_LEVEL = 2;
    const static string WORLD_SKELETON_XML = "../../data/world_skeletons/save_and_rescue_mission.xml";

    SARMission::SARMission() {
        this->buildWorld();
    }

    SARMission::~SARMission() {
        
    }

    void SARMission::buildWorld() {
        Mission::buildWorld();

        drawMainBuilding();

//        this->drawStairs(10, 3, HEIGHT_OF_GROUND_LEVEL, 4, 5, orientation::south_north);
//        this->drawStairs(20, 3, HEIGHT_OF_GROUND_LEVEL, 4, 5, orientation::north_south);
//        this->drawStairs(30, 3, HEIGHT_OF_GROUND_LEVEL, 4, 5, orientation::west_east);
//        this->drawStairs(40, 3, HEIGHT_OF_GROUND_LEVEL, 4, 5, orientation::east_west);
    }

    string SARMission::getWorldSkeletonFromXML() {
        return FileHandler::getFileContent(WORLD_SKELETON_XML);
    }

    void SARMission::drawMainBuilding() {
        drawGroundFloor();
    }

    void SARMission::drawGroundFloor() {
        drawRoom(2, 3, HEIGHT_OF_GROUND_LEVEL, 10, 4, 20);
        makeHole(1, 3, HEIGHT_OF_GROUND_LEVEL, 3, 3, 1);
        drawStairs(1, 18, HEIGHT_OF_GROUND_LEVEL, 3, 4, south_north);
        makeHole(1, 18, HEIGHT_OF_GROUND_LEVEL + 4 - 1, 3, 1, 3);
    }

} // namespace tomcat
