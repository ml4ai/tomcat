#include "TomcatMission.h"
#include "FileHandler.h"
#include "fmt/format.h"

using namespace malmo;
using namespace std;
using fmt::format;

namespace tomcat {

  TomcatMission::TomcatMission(int missionId) { this->missionId = missionId; }

  TomcatMission::~TomcatMission() {}

  void TomcatMission::buildWorld() {
    Mission::buildWorld();

    //Mission::insertTimeLimitInSeconds();

    if (this->requestVideo_switch) {
      this->insertVideoProducer();
    }

    if (this->observeRecentCommands_switch) {
      this->insertObserveRecentCommandsProducer();
    }

    if (this->observeHotBar_switch) {
      this->insertObserveHotBarProducer();
    }

    if (this->observeFullInventory_switch) {
      this->insertObserveFullInventoryProducer();
    }

    if (observeChat_switch) {
      this->insertObserveChatProducer();
    }
  }

  string TomcatMission::getWorldSkeletonFromXML() {
    string worldSkeletonXML = format(R"(
    <?xml version="1.0" encoding="UTF-8"?>
    <Mission xmlns="http://ProjectMalmo.microsoft.com" 
             xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"> 
      <About>
          <Summary>Search and Rescue</Summary>
      </About>
      <ServerSection>
          <ServerInitialConditions>
            <AllowSpawning>false</AllowSpawning>
          </ServerInitialConditions>
          <ServerHandlers>
            <FileWorldGenerator src="/Users/paulosoares/Desktop/ivilab/tomcat/data/worlds/{}" forceReset="true"/>
            <TomcatDecorator mission="{}" timeLimitInSeconds="{}"/>
          </ServerHandlers>
      </ServerSection>
      <AgentSection mode="Survival">
          <Name>Tomcat</Name>
          <AgentStart>
          </AgentStart>
          <AgentHandlers>
            <ContinuousMovementCommands turnSpeedDegs="840">
                <ModifierList type="deny-list">
                  <command>strafe</command>
                </ModifierList>
            </ContinuousMovementCommands>
          </AgentHandlers>
      </AgentSection>
    </Mission>)", this->getWorldFolder(), this->missionId, this->timeLimitInSeconds);

    return worldSkeletonXML;
  }

  string TomcatMission::getWorldFolder() {
      string folderName = "";
      switch (this->missionId) {
          case tutorial:
            folderName = "tutorial0.01";
            break;

          case sar:
              folderName = "sar0.01";
              break;
      }

      return folderName;
  }

} // namespace tomcat
