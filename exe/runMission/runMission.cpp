#include "Mission.h"
#include "utils.h"
#include <boost/program_options.hpp>
#include <string>

using namespace boost::program_options;
using namespace std;
using namespace tomcat;

// Function to deal with program options specified as environment variables.
// Right now this function is an identity mapping, but in the future it might
// get more complex. For example, if we want to ensure that ToMCAT environment
// variables are prefixed with TOMCAT_.
string mapper(string env_var) {
    set<string> valid_vars = {"self_report_interval", "difficulty"};
    return in(valid_vars, env_var) ? env_var : "";
}

options_description load_options() {
    options_description options("Allowed options");
    options.add_options()("help,h",
                          "Executable for running ToMCAT experiments.")(
        "mission",
        value<string>()->default_value("0"),
        "Mission ID or path to mission XML file.\n"
        "  0: Tutorial\n"
        "  1: Zombie Invasion\n"
        "  2: USAR (Singleplayer)\n"
        "  3: Procedurally generated mission")(
        "difficulty",
        value<unsigned int>()->default_value(5),
        "Level of difficulty.\n"
        "  3: Easy\n"
        "  5: Medium\n"
        "  7: Hard")(
        "time_limit",
        value<unsigned int>()->default_value(20),
        "Time limit for mission (in seconds).")(
        "self_report_interval",
        value<unsigned int>()->default_value(180),
        "Self-report prompt interval time (in seconds).")(
        "port,p",
        value<unsigned int>()->default_value(10000),
        "Port to control (>=10000)")("record_all",
                                     bool_switch()->default_value(false),
                                     "Activate all recordings except bitmaps")(
        "record_observations",
        bool_switch()->default_value(false),
        "Activate observation recordings")("record_commands",
                                           bool_switch()->default_value(false),
                                           "Activate command recordings")(
        "record_rewards",
        bool_switch()->default_value(false),
        "Activate reward recordings")("multiplayer",
                                      bool_switch()->default_value(false),
                                      "Run mission in multiplayer mode")(
        "uuid",
        value<string>()->default_value("0"),
        "If provided, set the UUID of the session at runtime.");

    return options;
}

variables_map
parse_parameters(options_description options, int argc, const char* argv[]) {
    variables_map vm;
    store(parse_command_line(argc, argv, options), vm);
    store(parse_environment(options, boost::function1<string, string>(mapper)),
          vm);
    notify(vm);

    return vm;
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
    unsigned int port_number = parameters_map["port"].as<unsigned int>();
    unsigned int time_limit_in_seconds =
        parameters_map["time_limit"].as<unsigned int>();
    unsigned int self_report_prompt_time_in_seconds =
        parameters_map["self_report_interval"].as<unsigned int>();
    unsigned int level_of_difficulty =
        parameters_map["difficulty"].as<unsigned int>();
    bool record_all = parameters_map["record_all"].as<bool>();
    bool record_observations = parameters_map["record_observations"].as<bool>();
    bool record_commands = parameters_map["record_commands"].as<bool>();
    bool record_rewards = parameters_map["record_rewards"].as<bool>();
    bool multiplayer = parameters_map["multiplayer"].as<bool>();
    string uuid = parameters_map["uuid"].as<string>();
    if (record_all) {
        record_observations = true;
        record_commands = true;
        record_rewards = true;
    }

    Mission mission = Mission(mission_id_or_path,
                              time_limit_in_seconds,
                              self_report_prompt_time_in_seconds,
                              level_of_difficulty,
                              port_number,
                              record_observations,
                              record_commands,
                              record_rewards,
                              multiplayer,
                              uuid);
    return mission;
}

int main(int argc, const char* argv[]) {
    options_description options = load_options();
    variables_map parameters_map = parse_parameters(options, argc, argv);

    if (are_parameters_ok(parameters_map, options)) {
        Mission mission = create_mission(parameters_map);
        try {
            mission.start();
        }
        catch (exception& e) {
            cerr << "Error starting mission: " << e.what();
            return EXIT_FAILURE;
        }
    }
    else {
        cerr << "Error with mission parameters";
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}
