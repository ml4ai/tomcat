#pragma once

#include "Mission.h"

namespace tomcat {

    /**
     * The SARMission class represents the Search and Rescue mission
     */
    class SARMission : public Mission 
    {
    public:
        /**
         * Constructor
         */
        SARMission();

        /**
         * Destructor
         */
        ~SARMission();

    protected:

        /**
         * Builds the world for the Search and Rescue mission
         */
        void buildWorld();

        /**
         * Retrieves the content of an XML which defines the skeleton of the world for the Search and Rescue mission
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
    };

} // namespace tomcat
