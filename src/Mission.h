#pragma once

#include "MissionSpec.h"
#include <string>

namespace tomcat {

  /**
   * The Mission interface represents an abstract Minecraft mission
   */
  class Mission {
  public:
    /**
     * Build the world with all the features and elements the mission must have.
     * This method must be overwritten by the subclasses of Mission to specify
     * the details of a concrete mission.
     */
    virtual void buildWorld();

    /**
     * Retrieves the object of class MissionSpec of the mission
     */
    malmo::MissionSpec getMissionSpec();

    /**
     * Defines the time limit for the mission
     * @param timeInSeconds - Time (in seconds) to the end of the mission
     */
    void setTimeLimitInSeconds(int timeInSeconds);

    /**
    * Requests video from MissionSpec
    * @param width - width of the video
    * @param height - height of the video
    */
    void requestVideo(unsigned int width, unsigned int height);

    void observeRecentCommands();

    void observeHotBar();

    void observeFullInventory();

    void observeChat();

  protected:
    /**
    * Retrieves the content of an XML which defines the skeleton of the world
    * for the Save and Rescue mission
    * @return
    */
    virtual std::string getWorldSkeletonFromXML() = 0;

    void insertTimeLimitInSeconds();

    void insertVideoProducer();

    void insertObserveRecentCommandsProducer();

    void insertObserveHotBarProducer();

    void insertObserveFullInventoryProducer();

    void insertObserveChatProducer();

    malmo::MissionSpec missionSpec;
    int timeLimitInSeconds;
    bool requestVideo_switch = false;
    unsigned int video_width = 640; 
    unsigned int video_height = 480;
    bool observeRecentCommands_switch = false;
    bool observeHotBar_switch = false;
    bool observeFullInventory_switch = false;
    bool observeChat_switch = false;
  };
} // namespace tomcat
