#include "LocalAgent.h"
#include <boost/program_options.hpp>
#include <fmt/format.h>
#include <string>


int main(int argc, const char *argv[]) {
  using namespace boost::program_options;
  using namespace std;
  using fmt::print;
  using namespace fmt::literals;

  // Program options
  options_description desc("Allowed options");
  string mission_xml;

  desc.add_options()
    ("help,h", "Executable for running ToMCAT experiments.")
    ("mission", value<string>(&mission_xml), "Path to mission XML file.")
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
    LocalAgent agent;
    agent.set_mission(mission_xml, vm["time_limit"].as<unsigned int>());
    agent.startMission(vm["port"].as<unsigned int>(), vm["activate_webcam"].as<bool>());
  }
  else {
    cout << desc << endl;
    return 1;
  }


  return EXIT_SUCCESS;
}
