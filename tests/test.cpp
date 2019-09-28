#include "LocalAgent.h"
#include <boost/program_options.hpp>
#include <fmt/format.h>
#include <string>


int main(int argc, const char *argv[]) {
  using namespace boost::program_options;
  using namespace std;
  using fmt::print;
  using namespace fmt::literals;
  using namespace tomcat;

  // Program options
  options_description desc("Allowed options");
  string mission_xml;

  desc.add_options()
    ("help,h", "Executable for running ToMCAT tests.")
    ("mission", value<string>(&mission_xml), "Path to mission XML file.")
    ("time_limit", value<unsigned int>()->default_value(20),
     "Time limit for mission.")
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
    agent.setMission(mission_xml, vm["time_limit"].as<unsigned int>());
  }
  else {
    cout << desc << endl;
    return 1;
  }


  return EXIT_SUCCESS;
}
