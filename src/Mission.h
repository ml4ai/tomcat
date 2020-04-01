#pragma once

#include "MissionSpec.h"
#include <AgentHost.h>
#include <boost/filesystem.hpp>
#include <string>
#include <unordered_map>

namespace tomcat {

  class LocalAgent; // Forward declaration to deal with circular dependency

  class TomcatMissionException : public std::exception {
  public:
    enum ErrorCode {
      CONNECTION_NOT_ESTABLISHED,
      TOMCAT_VAR_INEXISTENT,
      ERROR_STARTING_MISSION
    };

    TomcatMissionException(const std::string& message, ErrorCode error_code)
        : message(message), error_code(error_code) {}
    ~TomcatMissionException() throw() {}
    ErrorCode get_error_code() const { return this->error_code; }
    std::string get_message() const { return this->message; }
    const char* what() const throw() { return this->message.c_str(); }

  private:
    std::string message;
    ErrorCode error_code;
  };

  /**
   * The Mission interface represents an abstract Minecraft mission
   */
  class Mission {
  public:
    /**
     * Constructor
     * @param mission_id_or_path - Mission ID or path to an xml file with the
     * mission specifications
     * @param time_limit_in_seconds - Duration of the mission in seconds
     * @param self_report_prompt_time_in_seconds - Frequency of self-reports
     * @param port_number - Port number to connect with the Minecraft server
     * video
     * @param record_observations - Flag that activates observations recording
     * @param record_commands - Flag that activates commands recording
     * @param record_rewards - Flag that activates rewards recording
     * @param multiplayer - Flag that indicates a multiplayer mission
     * @param record_path - Path where recordings will be saved

     */
    Mission(std::string mission_id_or_path,
            unsigned int time_limit_in_seconds,
            unsigned int self_report_prompt_time_in_seconds,
            int port_number,
            bool record_observations,
            bool record_commands,
            bool record_rewards,
            bool multiplayer,
            std::string record_path = "./saved_data.tgz");

    /**
     * Destructor
     */
    ~Mission(){};

    /**
     * Adds a Tomcat agent as a listener of the mission
     * @param tomcat_agent - Tomcat agent
     */
    void add_listener(std::shared_ptr<LocalAgent> tomcat_agent);

    /**
     * Starts the mission
     * @return
     */
    void start();

    /**
     * Sends command to the host
     * @param command - Command to be executed
     */
    void send_command(std::string command);

  private:
    enum MissionId { TUTORIAL = 0, SAR = 1 };

    malmo::MissionSpec mission_spec;
    std::string mission_id_or_path;
    unsigned int time_limit_in_seconds;
    unsigned int self_report_prompt_time_in_seconds;
    int port_number;
    bool record_observations;
    bool record_commands;
    bool record_rewards;
    bool multiplayer;
    std::string record_path = "./saved_data.tgz";
    std::shared_ptr<malmo::AgentHost> minecraft_server;
    std::vector<std::shared_ptr<malmo::AgentHost>> minecraft_clients;
    std::shared_ptr<malmo::ClientPool> client_pool;
    std::vector<std::shared_ptr<LocalAgent>> tomcat_agents;

    /**
     * Creates the MissionSpec object based on the mission Id or XML file
     */
    void create_mission_spec();

    /**
     * Retrieves, from a mission id, the folder name where its hand-constructed
     * world is
     */
    inline static std::unordered_map<int, std::string> id_to_world_folder_map =
        {
            {TUTORIAL, "tutorial"},
            {SAR, "sar"},
    };

    /**
     * Creates a client pool for the server and each client
     */
    void create_client_pool();

    /**
     * Retrieves the content of an XML which defines the skeleton of the world
     * for the Search and Rescue mission
     * @return
     */
    std::string get_world_skeleton_from_xml();

    /**
     * Creates the collection of AgentSection tags for the clients in the
     * mission. These tags will be included in the mission spec xml
     * @return
     */
    std::string create_agent_section_tags();

    /**
     * Creates AgentHost objects in charge of communicate with the Minecraft
     * server and clients
     */
    void create_agent_hosts();

    /**
     * Establish connection to a Minecraft host
     */
    std::shared_ptr<malmo::AgentHost> connect_to_minecraft(int role);

    /**
     * Retrieves a MissionRecordSpec for the mission
     * @return
     */
    malmo::MissionRecordSpec get_mission_record_spec();

    /**
     * Wait until all the clients have started the mission
     */
    void safe_wait_to_start();

    /**
     * Observe the mission. This method corresponds to the mission main loop and
     * is executed while the mission is running
     */
    void observe();
  };

} // namespace tomcat
