#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include <fstream>
#include <glob/glob.h>
#include <iostream>
#include <map>

using namespace std;
namespace json = boost::json;
namespace fs = std::filesystem;

struct Results {
    map<string, int> ac_use_counts = {};
    map<string, int> intervention_type_counts = {};
    int n_files_processed = 0;
    int n_interventions = 0;
};

void process_message(Results& results, json::value& message) {
    if (message.at("topic") == "agent/intervention/ASI_UAZ_TA1_ToMCAT/chat") {
        results.n_interventions++;
        json::string explanation = message.at_pointer("/data/explanation").as_string();
    }
    return;
}

void process_file(Results& results, fs::path p) {
    ifstream infile(p);
    string line;
    size_t line_number = 0;
    json::value jv = {};
    json::object obj = {};
    while (getline(infile, line)) {
        line_number++;
        try {
            jv = json::parse(line);
        }
        catch (exception& e) {
            cerr << "Unable to parse line " << line_number << ": " << e.what()
                 << endl;
        }
        if (jv.get_object().contains("topic") &&
            jv.at("topic").as_string() == "agent/intervention/ASI_UAZ_TA1_ToMCAT/chat") {
            results.n_interventions++;
            json::string explanation = jv.at_pointer("/data/explanation/info").as_string();
        }
    }
    BOOST_LOG_TRIVIAL(info) << "Finished processing " << p;
    results.n_files_processed++;
}

int main(int argc, char* argv[]) {
    string data_dir = "/media/mule/projects/tomcat/protected/study-3_2022";
    Results results;
    for (auto& filepath : glob::glob(data_dir + "/*T00*UAZ*.metadata")) {
        process_file(results, filepath);
    }
    return EXIT_SUCCESS;
}
