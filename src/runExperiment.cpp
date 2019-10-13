#include "LocalAgent.h"
#include <boost/program_options.hpp>
#include <string>
#include <fmt/format.h>

int main(int argc, const char *argv[]) {
  using namespace boost::program_options;
  using namespace std;
  using fmt::print;
  using namespace fmt::literals;
  using namespace tomcat;

  // Program options
  options_description desc("Allowed options");
  string missionIdOrPathToXML;

  desc.add_options()
    ("help,h", "Executable for running ToMCAT experiments.")
    ("mission", value<string>(&missionIdOrPathToXML)->default_value("0"),
    "Id or path to mission XML file.\n0: Tutorial\n1: Search and Rescue\n2: Item Crafting\n3: Room Escape")
    ("time_limit", value<unsigned int>()->default_value(100),
     "Time limit for mission.")
    ("port,p", value<unsigned int>()->default_value(10000), "Port to control (>=10000)")
    ("activate_webcam,w", bool_switch()->default_value(false), "Activate webcam to detect face landmarks? (true=1 or false=0)")
    ("record-all", bool_switch()->default_value(false), "Activate all recordings except bitmaps")
    ("record-video", bool_switch()->default_value(false), "Activate video recordings")
    ("record-observations", bool_switch()->default_value(false), "Activate observation recordings")
    ("record-commands", bool_switch()->default_value(false), "Activate command recordings")
    ("record-rewards", bool_switch()->default_value(false), "Activate reward recordings")
    ("video_fps", value<unsigned int>()->default_value(20), "Frames per second for video recordings")
    ("video_bit_rate", value<int64_t>()->default_value(400000), "Bit rate for video recordings")
    ("record_path", value<string>()->default_value("./saved_data.tgz"), "Path to save recordings")
    ("video_width", value<unsigned int>()->default_value(640),"Width for video recordings")
    ("video_height", value<unsigned int>()->default_value(480), "Height for video recordings")
  ;

  variables_map vm;
  store(parse_command_line(argc, argv, desc), vm);
  notify(vm);
  if (vm.count("help")) {
    cout << desc << endl;
    return 1;
  }

  if (vm.count("mission")) {
    unsigned int timeLimitInSeconds = vm["time_limit"].as<unsigned int>();
    unsigned int portNumber = vm["port"].as<unsigned int>();
    unsigned int width = vm["video_width"].as<unsigned int>();
    unsigned int height = vm["video_height"].as<unsigned int>();
    unsigned int frames_per_second = vm["video_fps"].as<unsigned int>();
    int64_t bit_rate = vm["video_bit_rate"].as<int64_t>();
    string recordPath = vm["record_path"].as<string>();
    bool activateWebcam = vm["activate_webcam"].as<bool>();

    bool activateVideo;
    bool activateObsRec;
    bool activateComRec;
    bool activateRewRec;
 
    if (vm.count("record-all")) {
      activateVideo = true;
      activateObsRec = true;
      activateComRec = true;
      activateRewRec = true;

    } else {
      activateVideo = vm["record-video"].as<bool>();
      activateObsRec = vm["record-observations"].as<bool>();
      activateComRec = vm["record-commmands"].as<bool>();
      activateRewRec = vm["record-rewards"].as<bool>();

    }
 
    LocalAgent agent;
    agent.setMission(missionIdOrPathToXML, timeLimitInSeconds, width, height, activateVideo);
    agent.startMission(portNumber, activateWebcam, activateVideo, activateObsRec, activateComRec, activateRewRec, frames_per_second, bit_rate, recordPath);
  } else {
    cout << desc << endl;
    return 1;
  }

  return EXIT_SUCCESS;
}
