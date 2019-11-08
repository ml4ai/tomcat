#pragma once

#include "Mission.h"

namespace tomcat {

  /**
   * The TomcatMission class represents one of the standard ToMCAT missions
   */
  class TomcatMission : public Mission {
  public:
    /**
     * Constructor
     */
    TomcatMission(int missionId);

    /**
     * Destructor
     */
    ~TomcatMission();

  protected:
    /**
     * Builds the world for the ToMCAT mission
     */
    void buildWorld();

    /**
     * Retrieves the content of an XML which defines the skeleton of the
     * world for the ToMCAT mission
     * @return
     */
    std::string getWorldSkeletonFromXML();

    /**
     * Draws a giant ToMCAT sign using gold blocks, above the main building.
     */
    void drawTomcatSign();

    /**
     * Draws the main building of the mission
     */
    void drawMainBuilding();

    /**
     * Draws the ground floor of the main building
     */
    void drawGroundFloor();

    /**
     * Draws the second floor of the main building
     */
    void drawSecondFloor();

    int missionId;
  };

} // namespace tomcat
