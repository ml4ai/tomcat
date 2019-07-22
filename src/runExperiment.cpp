#include "LocalAgent.h"
#include <boost/program_options.hpp>
#include <fmt/core.h>
#include <string>


int main(int argc, const char *argv[]) {
  using namespace boost::program_options;
  using namespace std;
  using fmt::print;

  // Program options
  options_description desc("Allowed options");
  string mission_xml;
  desc.add_options()
    ("help", "Executable for running ToMCAT experiments.")
    ("mission", value<string>(&mission_xml), "Path to mission XML file.")
    ("port", value<int>()->default_value(10000), "Port to control (>=10000)")
  ;
  variables_map vm;
  store(parse_command_line(argc, argv, desc), vm);
  notify(vm);
  if (vm.count("help")) {
    cout << desc << endl;
    return 1;
  }

  if (vm.count("mission")) {
    LocalAgent agent;
    agent.set_mission(mission_xml);
    agent.startMission(vm["port"].as<int>());
  }
  else {
    cout << desc << endl;
    return 1;
  }


  return EXIT_SUCCESS;
}
