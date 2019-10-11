#include "MissionHandler.h"
#include "MissionFactory.h"

using namespace std;
using namespace malmo;

namespace tomcat {

    MissionHandler::MissionHandler() { }

    MissionHandler::~MissionHandler() { }

    void MissionHandler::setMission(string missionIdOrPathToXML) {
        this->mission = MissionFactory::create(missionIdOrPathToXML);
    }

    void MissionHandler::startMission() {
        this->mission->buildWorld();
    }

    void MissionHandler::setTimeLimitInSeconds(int timeInSeconds) {
        this->mission->setTimeLimitInSeconds(timeInSeconds);
    }

    MissionSpec MissionHandler::getMissionSpec()
    {
        return this->mission->getMissionSpec();
    }

} //namespace tomcat
