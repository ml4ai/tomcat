#pragma once

#include "Mission.h"
#include <string>

namespace tomcat {

    class MissionFactory {
    public:
        /**
         * Creates an object of a concrete mission class
         * @param missionIdOrPathToXML - Id or path to XML file of a predefined mission
         */
        static Mission* create(std::string missionIdOrPathToXML);
    };

} // namespace tomcat
