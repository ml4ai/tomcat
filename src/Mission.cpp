#include "Mission.h"
#include "FileHandler.h"
#include "LocalAgent.h"
#include <boost/filesystem.hpp>
#include <fmt/format.h>

using namespace malmo;
using namespace std;
using boost::filesystem::path;
using fmt::format;
using fmt::print;
using namespace std::chrono;
using namespace std::this_thread;

namespace tomcat {

  Mission::Mission(std::string mission_id_or_path,
                   unsigned int time_limit_in_seconds,
                   unsigned int self_report_prompt_time_in_seconds,
                   int port_number,
                   bool record_observations,
                   bool record_commands,
                   bool record_rewards,
                   std::string record_path
                   ) {

    this->mission_id_or_path = mission_id_or_path;
    this->time_limit_in_seconds = time_limit_in_seconds;
    this->self_report_prompt_time_in_seconds =
        self_report_prompt_time_in_seconds;
    this->port_number = port_number;
    this->record_observations = record_observations;
    this->record_commands = record_commands;
    this->record_rewards = record_rewards;
    this->record_path = record_path;
  }

  void Mission::add_listener(std::shared_ptr<LocalAgent> tomcat_agent) {
    this->tomcat_agents.push_back(tomcat_agent);
  }

  void Mission::start() {
    this->create_mission_spec();
    this->connect_to_host();
    this->init_external_sensors();
    this->observe();
  }

  void Mission::create_mission_spec() {
    path p(this->mission_id_or_path);
    if (p.extension() == ".xml") {
      string xml = FileHandler::getFileContent(this->mission_id_or_path);
      this->mission_spec = MissionSpec(xml, true);
      this->mission_spec.timeLimitInSeconds(this->time_limit_in_seconds);
    }
    else {
      string xml = get_world_skeleton_from_xml();
      this->mission_spec = MissionSpec(xml, true);
    }

    if (this->record_observations) {
      this->mission_spec.observeRecentCommands();
      this->mission_spec.observeHotBar();
      this->mission_spec.observeFullInventory();
      this->mission_spec.observeChat();
    }
  }

  string Mission::get_world_skeleton_from_xml() {
    if (!getenv("TOMCAT")) {
      throw TomcatMissionException(
          "TOMCAT environment variable does not exist.",
          TomcatMissionException::TOMCAT_VAR_INEXISTENT);
    }

    string xml = format(
        R"(
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
                <FileWorldGenerator
                  src="{}/data/worlds/{}"
                  forceReset="true"
                />
                <TomcatDecorator
                  mission="{}"
                  timeLimitInSeconds="{}"
                  selfReportPromptTimeInSeconds="{}"
                />
              </ServerHandlers>
          </ServerSection>
          <AgentSection mode="Adventure">
              <Name>Tomcat</Name>
              <AgentStart>
              </AgentStart>
              <AgentHandlers>
                <ObservationFromFullStats/>
                <ContinuousMovementCommands turnSpeedDegs="840">
                    <ModifierList type="deny-list">
                      <command>strafe</command>
                    </ModifierList>
                </ContinuousMovementCommands>
              </AgentHandlers>
          </AgentSection>
        </Mission>)",
        getenv("TOMCAT"),
        Mission::id_to_world_folder_map.at(std::stoi(this->mission_id_or_path)),
        this->mission_id_or_path,
        this->time_limit_in_seconds,
        this->self_report_prompt_time_in_seconds);

    return xml;
  }

  void Mission::connect_to_host() {
    int attempts = 0;
    bool connected = false;
    this->host = boost::make_shared<AgentHost>();
    do {
      try {
        ClientPool client_pool = this->get_client_pool();
        MissionRecordSpec mission_record_spec = this->get_mission_record_spec();
        this->host->startMission(
            this->mission_spec, client_pool, mission_record_spec, 0, "");
        connected = true;
      }
      catch (exception& e) {
        print("Error starting mission: {}", e.what());
        attempts += 1;
        // Give up after three attempts.
        if (attempts >= 3) {
          throw TomcatMissionException(
              "Could not establish connection with the host.",
              TomcatMissionException::CONNECTION_NOT_ESTABLISHED);
        }
        else {
          // Wait a second and try again.
          sleep_for(milliseconds(1000));
        }
      }
    } while (!connected);
  }

  MissionRecordSpec Mission::get_mission_record_spec() {
    MissionRecordSpec mission_record_spec(this->record_path);

    if (this->record_observations) {
      mission_record_spec.recordObservations();
    }

    if (this->record_commands) {
      mission_record_spec.recordCommands();
    }

    if (this->record_rewards) {
      mission_record_spec.recordRewards();
    }

    return mission_record_spec;
  }

  ClientPool Mission::get_client_pool() const {
    ClientPool client_pool;
    client_pool.add(ClientInfo("127.0.0.1", this->port_number));
    return client_pool;
  }

  void Mission::init_external_sensors() {
    WorldState worldState;
    do {
      sleep_for(milliseconds(100));
      worldState = this->host->getWorldState();
    } while (!worldState.has_mission_begun);
  }

  void Mission::observe() {
    WorldState worldState;
    do {
      sleep_for(milliseconds(10));
      for (auto& tomcat_agent : this->tomcat_agents) {
        tomcat_agent->observe_mission(*this);
      }
      worldState = this->host->getWorldState();
    } while (worldState.is_mission_running);
  }

  void Mission::send_command(string command) {
    this->host->sendCommand(command);
  }

} // namespace tomcat
