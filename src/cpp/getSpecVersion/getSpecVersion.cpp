#include <yaml-cpp/yaml.h>
#include <iostream>

int main(int argc, char* argv[]) {
    YAML::Node node = YAML::LoadFile(argv[1]);
    std::cout << node["info"]["version"] << std::endl;
    return 0;
}
