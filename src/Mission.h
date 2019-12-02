#pragma once

#include "MissionSpec.h"
#include <boost/filesystem.hpp>
#include <string>
#include <unordered_map>

namespace tomcat {

  /**
   * The Mission interface represents an abstract Minecraft mission
   */
  class Mission : public malmo::MissionSpec {
  public:
    /* ============
     * Constructors
     * ============ */

    Mission(){};
    Mission(std::string xml, bool validate)
        : malmo::MissionSpec(xml, validate){};

    /* =======================================================================
     *
     * Static method 'constructors'
     * ----------------------------
     *
     * These functions play a role similar to 'factory' classes, but with less
     * indirection.
     *
     * =======================================================================
     */

    static Mission fromMissionIdOrPathToXML(std::string missionIdOrPathToXML);
    static Mission from_mission_id(int missionID);
    static Mission from_XML_string(std::string xml);
    static Mission from_XML_file(std::string missionIdOrPathToXML);

    int getTimeLimitInSeconds() { return this->timeLimit; };
    void setTimeLimitInSeconds(int time_limit_in_seconds) {
      this->timeLimit = time_limit_in_seconds;
    };

  protected:
    /**
     * Retrieves the content of an XML which defines the skeleton of the world
     * for the Search and Rescue mission
     * @return
     */
    static std::string getWorldSkeletonFromXML(int mission_id);
    std::string getWorldFolder();

    int timeLimit = 10;
    enum MissionId { tutorial = 0, sar = 1 };
    inline static std::unordered_map<int, std::string> IdToWorldFolderMap = {
        {tutorial, "tutorial_0_0_1"},
        {sar, "sar_0_0_1"},
    };
  };
} // namespace tomcat
