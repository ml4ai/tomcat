#include "Mission.h"
#include "FileHandler.h"
#include "LocalAgent.h"
#include <boost/filesystem.hpp>
#include <fmt/format.h>
#include <sstream>
#include <ctime>

using namespace malmo;
using namespace std;
using boost::filesystem::path;
using fmt::format;
using fmt::print;
using namespace std::chrono;
using namespace std::this_thread;

namespace tomcat {

  Mission::Mission(string mission_id_or_path,
                   unsigned int time_limit_in_seconds,
                   unsigned int self_report_prompt_time_in_seconds,
                   int port_number,
                   bool record_observations,
                   bool record_commands,
                   bool record_rewards,
                   bool multiplayer,
                   string record_path
                   ) {

    this->mission_id_or_path = mission_id_or_path;
    this->time_limit_in_seconds = time_limit_in_seconds;
    this->self_report_prompt_time_in_seconds =
        self_report_prompt_time_in_seconds;
    this->port_number = port_number;
    this->record_observations = record_observations;
    this->record_commands = record_commands;
    this->record_rewards = record_rewards;
    this->multiplayer = multiplayer;
    this->record_path = record_path;
  }

  void Mission::add_listener(shared_ptr<LocalAgent> tomcat_agent) {
    this->tomcat_agents.push_back(tomcat_agent);
  }

  void Mission::start() {
    this->create_client_pool();
    this->create_mission_spec();
    this->create_agent_hosts();
    this->safe_wait_to_start();
    this->observe();
  }

  void Mission::create_client_pool() {
    this->client_pool = make_shared<ClientPool>();
    this->client_pool->add(ClientInfo("127.0.0.1", this->port_number));

    if (this->multiplayer) {
      // Add each one of the clients in the multiplayer mission
      // This is hardcoded but needs to be moved to a config file at some point
      this->client_pool->add(ClientInfo("127.0.0.1", 10001));
      this->client_pool->add(ClientInfo("127.0.0.1", 10002));
    }
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

    string agent_section_tags = this->create_agent_section_tags();

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
          {}
        </Mission>)",
        getenv("TOMCAT"),
        Mission::id_to_world_folder_map.at(stoi(this->mission_id_or_path)),
        this->mission_id_or_path,
        this->time_limit_in_seconds,
        this->self_report_prompt_time_in_seconds,
        agent_section_tags);

    return xml;
  }

  string Mission::create_agent_section_tags() {
    stringstream ss;
    string agent_name;

    for (boost::shared_ptr<ClientInfo> client : this->client_pool->clients) {
      if (agent_name.empty()) {
        agent_name = "tomcat";
      }
      else {
        agent_name = format("{}:{}", client->ip_address, client->control_port);
      }
      ss << format(R"(<AgentSection mode="Adventure">
              <Name>{}</Name>
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
          </AgentSection>)",
                   agent_name);
    }

    return ss.str();
  }

  void Mission::create_agent_hosts() {
    this->minecraft_server = this->connect_to_minecraft(0);

    for (int role = 1; role < this->client_pool->clients.size(); role++) {
      this->minecraft_clients.push_back(this->connect_to_minecraft(role));
    }
  }

  shared_ptr<AgentHost> Mission::connect_to_minecraft(int role) {
    MissionRecordSpec mission_record_spec = this->get_mission_record_spec();

    int attempts = 0;
    int max_attempts = 10;
    bool connected = false;
    shared_ptr<AgentHost> host = make_shared<AgentHost>();
    do {
      try {
        host->startMission(this->mission_spec,
                           *this->client_pool.get(),
                           mission_record_spec,
                           role,
                           "tomcat");
        connected = true;
      }
      catch (MissionException& e) {
          switch (e.getMissionErrorCode()) {
              case malmo::MissionException::MISSION_SERVER_WARMING_UP:
                  print("Server not quite ready yet - waiting...");
                  sleep_for(milliseconds(2000));
                  break;
              case malmo::MissionException::MISSION_INSUFFICIENT_CLIENTS_AVAILABLE:
                  print("Not enough available Minecraft instances running.");
                  attempts++;
                  if (attempts < max_attempts) {
                      print("Will wait in case they are starting up.", max_attempts - attempts, "attempts left.");
                      sleep_for(milliseconds(2000));
                  }
                  break;
              case malmo::MissionException::MISSION_SERVER_NOT_FOUND:
                  print("Server not found - has the mission with role 0 been started yet?");
                  attempts++;
                  if (attempts < max_attempts) {
                      print("Will wait and retry.", max_attempts - attempts, "attempts left.");
                      sleep_for(milliseconds(2000));
                  }
                  break;
              default:
                  print("Other error", e.getMessage());
                  print("Waiting will not help here - bailing immediately.");
                  throw TomcatMissionException(
                          "Could not establish connection with the host.",
                          TomcatMissionException::CONNECTION_NOT_ESTABLISHED);
          }

          if (attempts == max_attempts) {
              print("All chances used up - bailing now.");
              throw TomcatMissionException(
                      "Could not establish connection with the host.",
                      TomcatMissionException::CONNECTION_NOT_ESTABLISHED);
          }
      }
    } while (!connected);

    return host;
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

  void Mission::safe_wait_to_start() {
      int number_of_hosts = this->client_pool->clients.size();
      vector<bool> mission_has_begun(number_of_hosts, false);
      int max_seconds_to_start = 120;
      clock_t start_time = clock();
      clock_t current_time;
      bool mission_has_begun_for_all = false;
      int elapsed_time_in_seconds = 0;
      // List with all minecraft hosts: server and clients
      vector<shared_ptr<AgentHost>> hosts(number_of_hosts);
      copy ( this->minecraft_clients.begin(), this->minecraft_clients.end(), hosts.begin() );
      hosts.insert(hosts.begin(), this->minecraft_server);
      while (!mission_has_begun_for_all && elapsed_time_in_seconds < max_seconds_to_start) {
          for (int i = 0; i < number_of_hosts; i++) {
              WorldState world_state = hosts[i]->peekWorldState();
              if (!world_state.errors.empty()) {
                  print("Errors waiting for mission start:");
                  for (auto& error : world_state.errors) {
                      print(error->text);
                  }
                  throw TomcatMissionException(
                          "Could not start the mission.",
                          TomcatMissionException::ERROR_STARTING_MISSION);
              }
              mission_has_begun[i] = world_state.has_mission_begun;
              sleep_for(milliseconds(100)); // 0.1 seconds
          }

          current_time = clock();
          elapsed_time_in_seconds = int(current_time - start_time) / CLOCKS_PER_SEC;
          mission_has_begun_for_all = std::all_of(mission_has_begun.begin(), mission_has_begun.end(), [](bool v) { return v; });
      }

      if (elapsed_time_in_seconds >= max_seconds_to_start) {
          print("Timed out waiting for mission to begin. Bailing.");
          throw TomcatMissionException(
                  "Timed out waiting for mission to begin.",
                  TomcatMissionException::ERROR_STARTING_MISSION);
      }
  }

  void Mission::observe() {
    WorldState worldState;
    do {
      sleep_for(milliseconds(10));
      for (auto& tomcat_agent : this->tomcat_agents) {
        tomcat_agent->observe_mission(*this);
      }

      worldState = this->minecraft_server->getWorldState();
    } while (worldState.is_mission_running);
  }

  void Mission::send_command(string command) {
    this->minecraft_server->sendCommand(command);
  }

} // namespace tomcat
