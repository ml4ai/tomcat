#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include <iostream>
#include "Agent.hpp"
#include "FileAgent.hpp"

using namespace std;
namespace json = boost::json;


FileAgent::FileAgent(const json::object &config) : Agent(config){

}


// write to filesystem
void FileAgent::write(const string topic, json::object &message) {

    message["topic"] = topic;

    cout << "FileAgent::Write:  " << message << endl;
}
