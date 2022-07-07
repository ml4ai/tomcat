#include <boost/json.hpp>
#include <fstream>
#include <iostream>

using namespace std;
namespace json = boost::json;

int main(int argc, char* argv[]) {
    for (size_t i = 1; i < argc; i++) {
        ifstream infile(argv[i]);
        std::string line;
        size_t line_number = 0;
        while (getline(infile, line)) {
            line_number++;
            try {
                json::value jv = json::parse(line);
            }
            catch (exception& e) {
                cerr << "Unable to parse line " << line_number << ": " << e.what() << endl;
            }
        }
        cout << "Finished processing " << argv[i] << endl;
    }
    return EXIT_SUCCESS;
}
