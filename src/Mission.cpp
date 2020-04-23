#include "Mission.h"
#include "FileHandler.h"
#include "LocalAgent.h"
#include "utils.h"
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/filesystem.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <fmt/format.h>
#include <nlohmann/json.hpp>
#include <sstream>

using namespace malmo;
using namespace std;
using fmt::format;
using fmt::print;
using namespace std::chrono;
using namespace std::this_thread;
using json = nlohmann::json;
namespace pt = boost::posix_time;
namespace fs = boost::filesystem;

namespace tomcat {

  Mission::Mission(string mission_id_or_path,
                   unsigned int time_limit_in_seconds,
                   unsigned int self_report_prompt_time_in_seconds,
                   unsigned int level_of_difficulty,
                   int port_number,
                   bool record_observations,
                   bool record_commands,
                   bool record_rewards,
                   bool multiplayer) {

    this->mission_id_or_path = mission_id_or_path;
    this->time_limit_in_seconds = time_limit_in_seconds;
    this->self_report_prompt_time_in_seconds =
        self_report_prompt_time_in_seconds;
    this->level_of_difficulty = level_of_difficulty;
    this->port_number = port_number;
    this->record_observations = record_observations;
    this->record_commands = record_commands;
    this->record_rewards = record_rewards;
    this->multiplayer = multiplayer;
    boost::uuids::uuid u;
    this->uuid = boost::uuids::to_string(u);
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
    fs::path p(this->mission_id_or_path);
    if (p.extension() == ".xml") {
      string xml = FileHandler::getFileContent(this->mission_id_or_path);
      this->mission_spec = MissionSpec(xml, true);
      this->mission_spec.timeLimitInSeconds(this->time_limit_in_seconds);
    }
    else {
      string xml = this->get_world_skeleton_from_xml();
      this->mission_spec = MissionSpec(xml, true);
    }

    if (this->record_observations) {
      this->mission_spec.observeRecentCommands();
      this->mission_spec.observeChat();
    }
  }

  string Mission::get_world_skeleton_from_xml() {
    if (!getenv("TOMCAT")) {
      throw TomcatMissionException(
          "The TOMCAT environment variable has not been set!\n"
          "Please set it to the location of your local copy of the tomcat\n"
          "repository, or use the run_session script, which automatically sets "
          "it.",
          TomcatMissionException::TOMCAT_ENV_VAR_NOT_SET);
    }

    string agent_section_tags = this->create_agent_section_tags();

    string world_dir =
        Mission::id_to_world_folder_map.at(stoi(this->mission_id_or_path));

    string world_dir_path =
        format("{}/data/worlds/{}", getenv("TOMCAT"), world_dir);

    if (!fs::exists(fs::path(world_dir_path))) {
      throw TomcatMissionException(
          format("Requested world directory {} not found.\n"
                 "Please download the world files by executing the "
                 "{}/tools/download/tomcat_worlds script.\n"
                 "Note: The world file for mission ID 2 (USAR Singleplayer) is "
                 "currently only available to teams participating in DARPA's "
                 "ASIST program.",
                 world_dir_path,
                 getenv("TOMCAT")),
          TomcatMissionException::WORLD_DIR_NOT_FOUND);
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
                  src="{}"
                  forceReset="true"
                />
                <TomcatDecorator
                  mission="{}"
                  timeLimitInSeconds="{}"
                  selfReportPromptTimeInSeconds="{}"
                  levelOfDifficulty="{}"
                />
              </ServerHandlers>
          </ServerSection>
          {}
        </Mission>)",
        world_dir_path,
        this->mission_id_or_path,
        this->time_limit_in_seconds,
        this->self_report_prompt_time_in_seconds,
        this->level_of_difficulty,
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
      // For USAR_SINGLEPLAYER mission: <Placement x="-2165" y="52" z="175"/>

      ss << format(R"(<AgentSection mode="Adventure">
              <Name>{}</Name>
              <AgentStart>
              </AgentStart>
              <AgentHandlers>
                <ObservationFromASISTParticipant/>
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
          print(stderr, "Server not quite ready yet - waiting...");
          sleep_for(milliseconds(2000));
          break;
        case malmo::MissionException::MISSION_INSUFFICIENT_CLIENTS_AVAILABLE:
          print(stderr, "Not enough available Minecraft instances running.");
          attempts++;
          if (attempts < max_attempts) {
            print(stderr,
                  "Will wait in case they are starting up.",
                  max_attempts - attempts,
                  "attempts left.");
            sleep_for(milliseconds(2000));
          }
          break;
        case malmo::MissionException::MISSION_SERVER_NOT_FOUND:
          print(stderr,
                "Server not found - has the mission with role 0 been started "
                "yet?");
          attempts++;
          if (attempts < max_attempts) {
            print(stderr,
                  "Will wait and retry.",
                  max_attempts - attempts,
                  "attempts left.");
            sleep_for(milliseconds(2000));
          }
          break;
        default:
          print(stderr, "Other error", e.getMessage());
          print(stderr, "Waiting will not help here - bailing immediately.");
          throw TomcatMissionException(
              "Could not establish connection with the host.",
              TomcatMissionException::CONNECTION_NOT_ESTABLISHED);
        }

        if (attempts == max_attempts) {
          print(stderr, "All chances used up - bailing now.");
          throw TomcatMissionException(
              "Could not establish connection with the host.",
              TomcatMissionException::CONNECTION_NOT_ESTABLISHED);
        }
      }
    } while (!connected);

    return host;
  }

  MissionRecordSpec Mission::get_mission_record_spec() {
    MissionRecordSpec mission_record_spec;

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
    copy(this->minecraft_clients.begin(),
         this->minecraft_clients.end(),
         hosts.begin());
    hosts.insert(hosts.begin(), this->minecraft_server);
    while (!mission_has_begun_for_all &&
           elapsed_time_in_seconds < max_seconds_to_start) {
      for (int i = 0; i < number_of_hosts; i++) {
        WorldState world_state = hosts[i]->peekWorldState();
        if (!world_state.errors.empty()) {
          print(stderr, "Errors waiting for mission start:");
          for (auto& error : world_state.errors) {
            print(stderr, error->text);
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
      mission_has_begun_for_all = std::all_of(mission_has_begun.begin(),
                                              mission_has_begun.end(),
                                              [](bool v) { return v; });
    }

    if (elapsed_time_in_seconds >= max_seconds_to_start) {
      print(stderr, "Timed out waiting for mission to begin. Bailing.");
      throw TomcatMissionException(
          "Timed out waiting for mission to begin.",
          TomcatMissionException::ERROR_STARTING_MISSION);
    }
  }

  void Mission::observe() {
    WorldState worldState;
    do {
      // Minecraft normally runs at a fixed rate of 20 ticks per second
      // (https://minecraft.gamepedia.com/Tick), so we set the sleep duration to
      // 50 ms.
      sleep_for(milliseconds(50));
      for (auto& tomcat_agent : this->tomcat_agents) {
        tomcat_agent->observe_mission(*this);
      }

      worldState = this->minecraft_server->peekWorldState();
      if (worldState.observations.size() != 0) {
        json observation = json::parse(worldState.observations.at(0)->text);
        json header = {};
        string timestamp =
            pt::to_iso_extended_string(pt::microsec_clock::universal_time()) +
            "Z";
        header["timestamp"] = timestamp;
        header["message_type"] = "observation";
        header["version"] = "0.2";

        //# Message
        json metadata = {};
        metadata["trial_id"] = this->uuid;
        metadata["timestamp"] = timestamp;
        metadata["source"] = "human";
        metadata["sub_type"] = "state";
        metadata["version"] = "0.2";

        //# Data
        json data = {};
        data["name"] = "tomcat";
        json message = {};
        message["header"] = header;
        message["msg"] = metadata;
        message["data"] = observation;

        cout << message.dump() << endl;
      }

    } while (worldState.is_mission_running);
  }

  void Mission::send_command(string command) {
    this->minecraft_server->sendCommand(command);
  }

} // namespace tomcat
