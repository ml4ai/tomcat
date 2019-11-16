#include "Mission.h"
#include <fmt/format.h>

using namespace malmo;
using namespace std;

namespace tomcat {

  void Mission::buildWorld() {
    string xml = this->getWorldSkeletonFromXML();
    this->missionSpec = MissionSpec(xml, true);
    this->missionSpec.forceWorldReset();
  }

  MissionSpec Mission::getMissionSpec() { return this->missionSpec; }

  void Mission::setTimeLimitInSeconds(int timeInSeconds) {
    this->timeLimitInSeconds = timeInSeconds;
  }

  int Mission::getTimeLimitInSeconds() {
    return this->timeLimitInSeconds;
  }

  void Mission::insertTimeLimitInSeconds() {
    this->missionSpec.timeLimitInSeconds(this->timeLimitInSeconds);
  }

  void Mission::requestVideo(unsigned int width, unsigned int height) {
    this->requestVideo_switch = true;
    this->video_width = width;
    this->video_height = height;
  }

  void Mission::insertVideoProducer(){
    this->missionSpec.requestVideo(this->video_width,this->video_height);
  }

  void Mission::observeRecentCommands() {
    this->observeRecentCommands_switch = true;
  }

  void Mission::observeHotBar() {
    this->observeHotBar_switch = true;
  }

  void Mission::observeFullInventory() {
    this->observeFullInventory_switch = true;
  }

  void Mission::observeChat() {
    this->observeChat_switch = true;
  }

  void Mission::insertObserveRecentCommandsProducer() {
    this->missionSpec.observeRecentCommands();
  }

  void Mission::insertObserveHotBarProducer() {
    this->missionSpec.observeHotBar();
  }

  void Mission::insertObserveFullInventoryProducer() {
    this->missionSpec.observeFullInventory();
  }

  void Mission::insertObserveChatProducer() {
    this->missionSpec.observeChat();
  }


} // namespace tomcat
