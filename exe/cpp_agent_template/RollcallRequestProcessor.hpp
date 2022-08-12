#pragma once

#include <string>
#include <boost/json.hpp>
#include "Processor.hpp"

namespace json = boost::json;
using namespace std;


class RollcallRequestProcessor: public Processor {

    string get_name(){ return "rollcall_request";}

    void process(json::object read_from_bus);    
};
