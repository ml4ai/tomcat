#pragma once

#include <string>
#include <boost/json.hpp>
#include "Processor.hpp"

namespace json = boost::json;
using namespace std;


class RollcallProcessor: public Processor {

    string get_name(){ return "rollcall_request";}

};
