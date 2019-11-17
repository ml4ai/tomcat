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
     * Retrieves the name of the folder that contains the mission prebuilt world
     * @return
     */
    std::string getWorldFolder();

    enum MissionId { tutorial = 0, sar = 1};
    int missionId;
  };

} // namespace tomcat
