/**
 * This program is responsible for converting messages from the message bus to a
 * matricial format interpretable by a probabilistic model.
 */

#include <string>

#include <boost/program_options.hpp>

#include "converter/TA3MessageConverter.h"

using namespace tomcat::model;
using namespace std;
namespace po = boost::program_options;

void convert_ta3_messages(const string& map_config_path,
                          const string& messages_input_dir,
                          const string& output_dir) {
    TA3MessageConverter converter(map_config_path);
    converter.convert_offline(messages_input_dir, output_dir);
}

int main(int argc, char* argv[]) {
    string map_config_path;
    string messages_dir;
    string output_dir;

    po::options_description desc("Allowed options");
    desc.add_options()("help,h", "Produce this help message")(
        "map_config",
        po::value<string>(&map_config_path)
            ->default_value("../../data/maps/ta3/falcon_v1.0.json"),
        "Path to the json file containing the map configuration.")(
        "messages_dir",
        po::value<string>(&messages_dir)
            ->default_value("../../data/messages/ta3/falcon"),
        "Directory where the files with the messages are stored.")(
        "output_dir",
        po::value<string>(&output_dir)
            ->default_value("../../data/samples/ta3/falcon/human/v2"),
        "Directory where the files with evidence extracted from the messages "
        "must be saved.");

    po::variables_map vm;
    po::store(po::parse_command_line(argc, argv, desc), vm);
    po::notify(vm);
    if (vm.count("help") || !vm.count("messages_dir") ||
        !vm.count("output_dir")) {
        cout << desc << "\n";
        return 1;
    }

    convert_ta3_messages(map_config_path, messages_dir, output_dir);
}
