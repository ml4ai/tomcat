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

    void MissionHandler::requestVideo(unsigned int width, unsigned int height) {
      this->mission->requestVideo(width,height);
    }

    void MissionHandler::observeRecentCommands() {
      this->mission->observeRecentCommands();
    }

    void MissionHandler::observeHotBar() {
      this->mission->observeHotBar();
    }

    void MissionHandler::observeFullInventory() {
      this->mission->observeFullInventory();
    }

    void MissionHandler::observeChat() {
      this->mission->observeChat();
    }

} //namespace tomcat
