#pragma once

#include "Mission.h"
#include "MissionSpec.h"
#include <string>

namespace tomcat {

    class MissionHandler {
    private:
        Mission *mission;

    public:
        /** 
         * Constructor 
         */
        MissionHandler();

        /** 
         * Destructor 
         */
        ~MissionHandler();

        /** 
         * Creates a mission
         * @param missionIdOrPathToXML - Id or path to XML file of a predefined mission
         */
        void setMission(std::string missionIdOrPathToXML);

        /**
         * Fills the missionSpec object with the skeleton of the world
         */
        void startMission();

        /**
         * Defines the time limit for the current mission
         * @param timeInSeconds - Time (in seconds) to the end of the mission
         */
        void setTimeLimitInSeconds(int timeInSeconds);

        /** 
         * Retrieves the object of class MissionSpec of the current mission 
         */
        malmo::MissionSpec getMissionSpec();

        /**
         * Requests video
         * @param width - width of video
         * @param height - height of video
         */
        void requestVideo(unsigned int width, unsigned int height);      

        void observeRecentCommands();

        void observeHotBar();

        void observeFullInventory();

        void observeChat();

    };   
    
} // namespace tomcat
