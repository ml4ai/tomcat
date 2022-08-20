#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "FileAgent.hpp"

using namespace std;
namespace json = boost::json;
using namespace std::chrono;


FileAgent::FileAgent(json::object config): Agent(config) {


}

// write to filesystem
void FileAgent::write(string topic, json::object message) {
    message["topic"] = topic;

    cout << "FileAgent::Writing " << message << endl;
}
