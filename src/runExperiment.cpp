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
    ("mission", value<string>(&missionIdOrPathToXML)->default_value("1"),
    "Id or path to mission XML file.\n1: Search and Rescue\n2: Item Crafting\n3: Room Escape")
    ("time_limit", value<unsigned int>()->default_value(20),
     "Time limit for mission.")
    ("port,p", value<unsigned int>()->default_value(10000), "Port to control (>=10000)")
    ("activate_webcam,w", value<bool>()->default_value(false), "Activate webcam to detect face landmarks? (true=1 or false=0)")
  ;

  variables_map vm;
  store(parse_command_line(argc, argv, desc), vm);
  notify(vm);
  if (vm.count("help") || argc == 1) {
    cout << desc << endl;
    return 1;
  }

  if (vm.count("mission")) {
      unsigned int timeLimitInSeconds =  vm["time_limit"].as<unsigned int>();
      unsigned int portNumber =  vm["port"].as<unsigned int>();
      bool activateWebcam = vm["activate_webcam"].as<bool>();

      LocalAgent agent;
      agent.setMission(missionIdOrPathToXML, timeLimitInSeconds);
      agent.startMission(portNumber, activateWebcam);
  } else {
    cout << desc << endl;
    return 1;
  }

  return EXIT_SUCCESS;
}
