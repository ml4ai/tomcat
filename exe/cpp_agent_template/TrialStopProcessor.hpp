#pragma once

#include <string>
#include <boost/json.hpp>
#include "Processor.hpp"

namespace json = boost::json;
using namespace std;


/** trial stop message */
class TrialStopProcessor: public Processor {

    public:
    string get_name() override;

};
