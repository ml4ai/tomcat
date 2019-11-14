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

    Mission::insertTimeLimitInSeconds();

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
            <FileWorldGenerator src="/Users/paulosoares/Desktop/ivilab/tomcat/data/worlds/SARV01" forceReset="true"/>
            <TomcatDecorator mission="{}" timeLimitInSeconds="{}"/>
          </ServerHandlers>
      </ServerSection>
      <AgentSection mode="Survival">
          <Name>Tomcat</Name>
          <AgentStart>
            <Placement x="22" y="64" z="73" yaw = "-90"/>
          </AgentStart>
          <AgentHandlers>
            <ContinuousMovementCommands turnSpeedDegs="840">
                <ModifierList type="deny-list">
                  <command>strafe</command>
                </ModifierList>
            </ContinuousMovementCommands>
          </AgentHandlers>
      </AgentSection>
    </Mission>)", this->missionId, this->timeLimitInSeconds);

    return worldSkeletonXML;
  }

} // namespace tomcat
