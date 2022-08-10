#include "Processor.hpp"
#include <boost/json.hpp>
#include <boost/log/trivial.hpp>

using namespace std;
namespace json = boost::json;


Processor::Processor(string name, json::object config):
    Message(name, config)
{

}

