#include <boost/next_prior.hpp>
#include <iostream>
#include <yaml-cpp/yaml.h>
#include <boost/program_options.hpp>
#include <boost/filesystem.hpp>

using namespace std;
namespace po = boost::program_options;
namespace fs = boost::filesystem;

int main(int argc, char* argv[]) {
    po::options_description desc("getSpecVersion options");
    string filename;
    desc.add_options()
        ("help,h", "Produce this help message and exit.")
        ("spec", po::value<string>(&filename)->required(), "Path to the spec YAML file.")
    ;
    po::variables_map vm;
    po::store(po::parse_command_line(argc, argv, desc), vm);

    if (vm.count("help")) {
        cout << desc << endl;
        return 1;
    }

    try {
        po::notify(vm);
    } catch (po::required_option& e) {
        cerr << "getSpecVersion error: " << e.what() << endl;
        return 1;
    }

    if (!fs::exists(filename)) {
        cerr << "getVersionSpec error: The given input spec YAML file \""
                << filename << "\" does not exist or is not a valid file." << endl;
        return 1;
    }

    YAML::Node node = YAML::LoadFile(filename);
    cout << node["info"]["version"] << endl;

    return 0;
}
