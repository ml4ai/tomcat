#include "LocalAgent.h"
#include "Mission.h"
#include "utils.h"
#include <boost/program_options.hpp>
#include <fmt/format.h>
#include <string>

using namespace boost::program_options;
using namespace std;
using fmt::print, tomcat::get_timestamp;
using namespace fmt::literals;
using namespace tomcat;

options_description load_options() {
  options_description options("Allowed options");
  options.add_options()("help,h", "Executable for running ToMCAT experiments.")(
      "mission",
      value<string>()->default_value("0"),
      "Id or path to mission XML file.\n0: Tutorial\n1: Zombie Invasion")(
      "time_limit",
      value<unsigned int>()->default_value(20),
      "Time limit for mission (in seconds).")(
      "self_report",
      value<unsigned int>()->default_value(180),
      "Self-report prompt interval time (in seconds).")(
      "port,p",
      value<unsigned int>()->default_value(10000),
      "Port to control (>=10000)")("activate_webcam,w",
                                   bool_switch()->default_value(false),
                                   "Activate webcam to detect face landmarks.")(
      "record_all",
      bool_switch()->default_value(false),
      "Activate all recordings except bitmaps")(
      "record_observations",
      bool_switch()->default_value(false),
      "Activate observation recordings")("record_commands",
                                         bool_switch()->default_value(false),
                                         "Activate command recordings")(
      "record_rewards",
      bool_switch()->default_value(false),
      "Activate reward recordings")(
      "record_path",
      value<string>()->default_value("./saved_data_" + get_timestamp() +
                                     ".tgz"),
      "Path to save Malmo data");

  return options;
}

variables_map
parse_parameters(options_description options, int argc, const char* argv[]) {
  variables_map parameters_map;
  store(parse_command_line(argc, argv, options), parameters_map);
  notify(parameters_map);

  return parameters_map;
}

bool are_parameters_ok(variables_map parameters_map,
                       options_description options) {
  if (parameters_map.count("help")) {
    cout << options << endl;
    return false;
  }
  else if (!parameters_map.count("mission")) {
    cout << options << endl;
    return false;
  }

  return true;
}

Mission create_mission(variables_map parameters_map) {
  string mission_id_or_path = parameters_map["mission"].as<string>();
  string record_path = parameters_map["record_path"].as<string>();
  unsigned int port_number = parameters_map["port"].as<unsigned int>();
  unsigned int video_width = parameters_map["video_width"].as<unsigned int>();
  unsigned int video_height = parameters_map["video_height"].as<unsigned int>();
  unsigned int frames_per_second =
      parameters_map["video_fps"].as<unsigned int>();
  unsigned int time_limit_in_seconds =
      parameters_map["time_limit"].as<unsigned int>();
  unsigned int self_report_prompt_time_in_seconds =
      parameters_map["self_report"].as<unsigned int>();
  int64_t bit_rate = parameters_map["video_bit_rate"].as<int64_t>();
  bool activate_webcam = parameters_map["activate_webcam"].as<bool>();
  bool record_all = parameters_map["record_all"].as<bool>();
  bool record_video = parameters_map["record_video"].as<bool>();
  bool record_audio = parameters_map["record_audio"].as<bool>();
  string audio_record_path = parameters_map["audio_record_path"].as<string>();
  bool record_observations = parameters_map["record_observations"].as<bool>();
  bool record_commands = parameters_map["record_commands"].as<bool>();
  bool record_rewards = parameters_map["record_rewards"].as<bool>();

  if (record_all) {
    record_observations = true;
    record_commands = true;
    record_rewards = true;
  }

  Mission mission = Mission(mission_id_or_path,
                            time_limit_in_seconds,
                            self_report_prompt_time_in_seconds,
                            port_number,
                            record_observations,
                            record_commands,
                            record_rewards,
                            record_path);
  return mission;
}

int main(int argc, const char* argv[]) {
  options_description options = load_options();
  variables_map parameters_map = parse_parameters(options, argc, argv);

  if (are_parameters_ok(parameters_map, options)) {
    Mission mission = create_mission(parameters_map);
    try {
      std::shared_ptr<LocalAgent> tomcat_agent = std::make_shared<LocalAgent>();
      mission.add_listener(tomcat_agent);
      mission.start();
    }
    catch (exception& e) {
      print("Error starting mission: {}", e.what());
      return EXIT_FAILURE;
    }
  }
  else {
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
