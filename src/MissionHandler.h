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
         * Defines the time limit for the current mission
         * @param timeInSeconds - Time (in seconds) to the end of the mission
         */
        void setTimeLimitInSeconds(int timeInSeconds);

        /** 
         * Retrieves the object of class MissionSpec of the current mission 
         */
        malmo::MissionSpec getMissionSpec();
    
    };   
    
} // namespace tomcat
