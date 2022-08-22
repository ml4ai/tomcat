#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include <iostream>
#include "FileAgent.hpp"

using namespace std;
namespace json = boost::json;


// write to filesystem
void FileAgent::write(const string topic, json::object &message) {

    message["topic"] = topic;

    cout << "FileAgent::Write:  " << message << endl;
}
