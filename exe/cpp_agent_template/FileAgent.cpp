#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "FileAgent.hpp"

using namespace std;
namespace json = boost::json;
using namespace std::chrono;


FileAgent::FileAgent(json::object config): Agent() {

    configure(config);

    // Report subscriptions and publications
    for(string topic : get_input_topics()) {
        cout << "Subscribed to: " << topic << endl;
    }

    // report output topics
    for(string topic : get_output_topics()) {
        cout << "Publishing on: " << topic << endl;
    }
}

// write to filesystem
void FileAgent::write(string topic, json::object message) {
    message["topic"] = topic;

    cout << "FileAgent::Writing " << message << endl;
}
