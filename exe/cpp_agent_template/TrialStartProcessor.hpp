#pragma once

#include <string>
#include <boost/json.hpp>
#include "Processor.hpp"

namespace json = boost::json;
using namespace std;


class TrialStartProcessor: public Processor {

    public:
    string get_name(){ return "trial_start";}

    void process(json::object read_from_bus);
};
