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
    enum ErrorCode { CONNECTION_NOT_ESTABLISHED, TOMCAT_VAR_INEXISTENT };

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
     */
    Mission(std::string mission_id_or_path,
            unsigned int time_limit_in_seconds,
            unsigned int self_report_prompt_time_in_seconds,
            int port_number,
            int frames_per_second,
            bool record_observations,
            bool record_commands,
            bool record_rewards,
            std::string record_path = "./saved_data.tgz",
            );

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
    boost::shared_ptr<malmo::AgentHost> host;
    std::string mission_id_or_path;
    unsigned int time_limit_in_seconds;
    unsigned int self_report_prompt_time_in_seconds;
    unsigned int video_width;
    unsigned int video_height;
    int port_number;
    int frames_per_second;
    int64_t bit_rate;
    bool record_video;
    bool record_observations;
    bool activate_webcam;
    bool record_audio;
    bool record_commands;
    bool record_rewards;
    std::string record_path = "./saved_data.tgz";
    std::string audio_record_path = "audio_recording.wav";
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
     * Retrieves the content of an XML which defines the skeleton of the world
     * for the Search and Rescue mission
     * @return
     */
    std::string get_world_skeleton_from_xml();

    /**
     * Establish connection with Minecraft host
     */
    void connect_to_host();

    /**
     * Retrieves a MissionRecordSpec for the mission
     * @return
     */
    malmo::MissionRecordSpec get_mission_record_spec();

    /**
     * Retrieves a client pool for the mission
     * @return
     */
    malmo::ClientPool get_client_pool() const;

    /**
     * Initialize external sensors
     */
    void init_external_sensors();

    /**
     * Observe the mission. This method corresponds to the mission main loop and
     * is executed while the mission is running
     */
    void observe();

    /**
     * Clean up processes related to external sensors
     */
    void finalize_external_sensors();
  };

} // namespace tomcat
