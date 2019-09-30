#include "SARMission.h"
#include "FileHandler.h"

using namespace malmo;
using namespace std;

namespace tomcat {

const static int HEIGHT_OF_GROUND_LEVEL = 2;
const static string WORLD_SKELETON_XML = R"(
    <?xml version="1.0" encoding="UTF-8"?>
    <Mission xmlns="http://ProjectMalmo.microsoft.com" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
      <About>
          <Summary>Search and Rescue</Summary>
      </About>
      <ServerSection>
          <ServerInitialConditions>
            <AllowSpawning>false</AllowSpawning>
          </ServerInitialConditions>
          <ServerHandlers>
            <FlatWorldGenerator generatorString="3;2*2;1;village" />
          </ServerHandlers>
      </ServerSection>
      <AgentSection mode="Survival">
          <Name>Tomcat</Name>
          <AgentStart>
            <Placement x="0" y="2.0" z="0" />
          </AgentStart>
          <AgentHandlers>
            <ContinuousMovementCommands turnSpeedDegs="840">
                <ModifierList type="deny-list">
                  <command>strafe</command>
                </ModifierList>
            </ContinuousMovementCommands>
          </AgentHandlers>
      </AgentSection>
    </Mission>)";

const static int FLOOR_HEIGHT = 5;
const static int BUILDING_WIDTH = 20;
const static int BUILDING_DEPTH = 30;
const static int BUILDING_SOUTHWEST_X_POSITION = 2;
const static int BUILDING_SOUTHWEST_Z_POSITION = 30;
const static int STAIRS_WIDTH = 3;
const static int MAIN_ENTRANCE_WIDTH = 3;

SARMission::SARMission() { this->buildWorld(); }

SARMission::~SARMission() {}

void SARMission::buildWorld() {
  Mission::buildWorld();

  this->drawMainBuilding();
  this->drawTomcatSign();

  // Put a zombie on the ground floor!
  this->drawEntity(BUILDING_SOUTHWEST_X_POSITION - 10,
                   HEIGHT_OF_GROUND_LEVEL,
                   BUILDING_SOUTHWEST_Z_POSITION + 20,
                   "Zombie");

  this->drawEntity(BUILDING_SOUTHWEST_X_POSITION - 10,
                   HEIGHT_OF_GROUND_LEVEL + FLOOR_HEIGHT,
                   BUILDING_SOUTHWEST_Z_POSITION + 20,
                   "Villager");
  for (unsigned short int i = 0; i < 5; i++) {
    this->drawTree(-20 - 5 * i, 20);
  }
}

string SARMission::getWorldSkeletonFromXML() { return WORLD_SKELETON_XML; }

void SARMission::drawTomcatSign() {
  int tomcatX = BUILDING_SOUTHWEST_X_POSITION + 10;
  int tomcatZ = BUILDING_SOUTHWEST_Z_POSITION;
  int tomcatY = 2 * FLOOR_HEIGHT + 5;
  int tomcatWidth = 7;

  string tomcatBlockType = "gold_block";

  // Letter T
  this->drawPlane(
      tomcatX, tomcatZ, tomcatY, 1, 2 * FLOOR_HEIGHT, 1, tomcatBlockType);

  this->drawPlane(tomcatX + tomcatWidth / 2,
                  tomcatZ,
                  tomcatY + 2 * FLOOR_HEIGHT - 1,
                  tomcatWidth,
                  1,
                  1,
                  tomcatBlockType);

  // Letter O
  tomcatX = tomcatX - tomcatWidth / 2 - 2;
  this->drawWall(tomcatX,
                 tomcatZ,
                 tomcatY,
                 tomcatWidth,
                 2 * FLOOR_HEIGHT,
                 orientation::west_east,
                 tomcatBlockType);
  this->makeHole(tomcatX - 1,
                 tomcatZ,
                 tomcatY + 1,
                 tomcatWidth - 2,
                 2 * FLOOR_HEIGHT - 2,
                 1);

  // Letter M
  tomcatX = tomcatX - tomcatWidth - 1;
  this->drawPlane(
      tomcatX, tomcatZ, tomcatY, 1, 2 * FLOOR_HEIGHT, 1, tomcatBlockType);
  this->drawPlane(tomcatX - 1,
                  tomcatZ,
                  tomcatY + 2 * FLOOR_HEIGHT - 1,
                  1,
                  1,
                  1,
                  tomcatBlockType);
  this->drawPlane(tomcatX - 2,
                  tomcatZ,
                  tomcatY + 2 * FLOOR_HEIGHT - 2,
                  1,
                  1,
                  1,
                  tomcatBlockType);
  this->drawPlane(tomcatX - 3,
                  tomcatZ,
                  tomcatY + 2 * FLOOR_HEIGHT - 3,
                  1,
                  1,
                  1,
                  tomcatBlockType);
  this->drawPlane(tomcatX - 4,
                  tomcatZ,
                  tomcatY + 2 * FLOOR_HEIGHT - 2,
                  1,
                  1,
                  1,
                  tomcatBlockType);
  this->drawPlane(tomcatX - 5,
                  tomcatZ,
                  tomcatY + 2 * FLOOR_HEIGHT - 1,
                  1,
                  1,
                  1,
                  tomcatBlockType);
  this->drawPlane(tomcatX - tomcatWidth + 1,
                  tomcatZ,
                  tomcatY,
                  1,
                  2 * FLOOR_HEIGHT,
                  1,
                  tomcatBlockType);

  // Letter C
  tomcatX = tomcatX - tomcatWidth - 1;
  this->drawWall(tomcatX,
                 tomcatZ,
                 tomcatY,
                 tomcatWidth,
                 2 * FLOOR_HEIGHT,
                 orientation::west_east,
                 tomcatBlockType);
  this->makeHole(
      tomcatX - 1, tomcatZ, tomcatY + 1, tomcatWidth, 2 * FLOOR_HEIGHT - 2, 1);

  // Letter A
  tomcatX = tomcatX - tomcatWidth - 1;
  this->drawPlane(
      tomcatX, tomcatZ, tomcatY, 1, 2 * FLOOR_HEIGHT, 1, tomcatBlockType);
  this->drawPlane(tomcatX - 1,
                  tomcatZ,
                  tomcatY + 2 * FLOOR_HEIGHT - 1,
                  tomcatWidth - 1,
                  1,
                  1,
                  tomcatBlockType);
  this->drawPlane(tomcatX - 1,
                  tomcatZ,
                  tomcatY + 2 * FLOOR_HEIGHT - 6,
                  tomcatWidth - 1,
                  1,
                  1,
                  tomcatBlockType);
  this->drawPlane(tomcatX - tomcatWidth + 1,
                  tomcatZ,
                  tomcatY,
                  1,
                  2 * FLOOR_HEIGHT,
                  1,
                  tomcatBlockType);

  // Letter T
  tomcatX = tomcatX - 3 * tomcatWidth / 2 - 1;
  this->drawPlane(
      tomcatX, tomcatZ, tomcatY, 1, 2 * FLOOR_HEIGHT, 1, tomcatBlockType);
  this->drawPlane(tomcatX + tomcatWidth / 2,
                  tomcatZ,
                  tomcatY + 2 * FLOOR_HEIGHT - 1,
                  tomcatWidth,
                  1,
                  1,
                  tomcatBlockType);
}

void SARMission::drawMainBuilding() {
  this->drawGroundFloor();
  this->drawSecondFloor();
}

void SARMission::drawSecondFloor() {
  this->drawRoom(BUILDING_SOUTHWEST_X_POSITION,
                 BUILDING_SOUTHWEST_Z_POSITION,
                 HEIGHT_OF_GROUND_LEVEL + FLOOR_HEIGHT,
                 BUILDING_WIDTH,
                 FLOOR_HEIGHT,
                 BUILDING_DEPTH);
  this->drawStairs(BUILDING_SOUTHWEST_X_POSITION - 1,
                   FLOOR_HEIGHT + BUILDING_SOUTHWEST_Z_POSITION - 1,
                   HEIGHT_OF_GROUND_LEVEL + FLOOR_HEIGHT,
                   STAIRS_WIDTH,
                   FLOOR_HEIGHT,
                   north_south);

  // Make hole in the roof to open an access to the next floor
  this->makeHole(BUILDING_SOUTHWEST_X_POSITION - 1,
                 BUILDING_SOUTHWEST_Z_POSITION + 1,
                 HEIGHT_OF_GROUND_LEVEL + 2 * FLOOR_HEIGHT - 1,
                 STAIRS_WIDTH,
                 1,
                 FLOOR_HEIGHT - 1);

  // Make holes in the lateral walls to receive light from outside
  this->makeHole(BUILDING_SOUTHWEST_X_POSITION - BUILDING_WIDTH - 1,
                 BUILDING_SOUTHWEST_Z_POSITION + 2,
                 HEIGHT_OF_GROUND_LEVEL + FLOOR_HEIGHT + 2,
                 1,
                 2,
                 BUILDING_DEPTH - 4);
  this->makeHole(BUILDING_SOUTHWEST_X_POSITION,
                 BUILDING_SOUTHWEST_Z_POSITION + 6,
                 HEIGHT_OF_GROUND_LEVEL + FLOOR_HEIGHT + 2,
                 1,
                 2,
                 BUILDING_DEPTH - FLOOR_HEIGHT - 6);
}

void SARMission::drawGroundFloor() {
  this->drawRoom(BUILDING_SOUTHWEST_X_POSITION,
                 BUILDING_SOUTHWEST_Z_POSITION,
                 HEIGHT_OF_GROUND_LEVEL,
                 BUILDING_WIDTH,
                 FLOOR_HEIGHT,
                 BUILDING_DEPTH);
  // Make hole in the wall for the main entrance of the floor
  this->makeHole(BUILDING_SOUTHWEST_X_POSITION - 1,
                 BUILDING_SOUTHWEST_Z_POSITION,
                 HEIGHT_OF_GROUND_LEVEL,
                 MAIN_ENTRANCE_WIDTH,
                 FLOOR_HEIGHT - 1,
                 1);
  this->drawStairs(BUILDING_SOUTHWEST_X_POSITION - 1,
                   BUILDING_SOUTHWEST_Z_POSITION + BUILDING_DEPTH -
                       FLOOR_HEIGHT - 1,
                   HEIGHT_OF_GROUND_LEVEL,
                   STAIRS_WIDTH,
                   FLOOR_HEIGHT,
                   south_north);
  // Make hole in the roof to open an access to the next floor
  this->makeHole(BUILDING_SOUTHWEST_X_POSITION - 1,
                 BUILDING_SOUTHWEST_Z_POSITION + BUILDING_DEPTH - FLOOR_HEIGHT -
                     1,
                 HEIGHT_OF_GROUND_LEVEL + FLOOR_HEIGHT - 1,
                 STAIRS_WIDTH,
                 1,
                 FLOOR_HEIGHT - 1);
  // Make holes in the lateral walls to receive light from outside
  this->makeHole(BUILDING_SOUTHWEST_X_POSITION - BUILDING_WIDTH - 1,
                 BUILDING_SOUTHWEST_Z_POSITION + 2,
                 HEIGHT_OF_GROUND_LEVEL + 2,
                 1,
                 2,
                 BUILDING_DEPTH - 4);
  this->makeHole(BUILDING_SOUTHWEST_X_POSITION,
                 BUILDING_SOUTHWEST_Z_POSITION + 2,
                 HEIGHT_OF_GROUND_LEVEL + 2,
                 1,
                 2,
                 BUILDING_DEPTH - FLOOR_HEIGHT - 4);
}

} // namespace tomcat
