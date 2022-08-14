#pragma once

#include <string>
#include <boost/json.hpp>
#include "Processor.hpp"

namespace json = boost::json;
using namespace std;


class RollcallRequestProcessor: public Processor {

    string get_subscription_name() override { return "rollcall_request"; }
    string get_publication_name() override { return "rollcall_response"; }

    void process_input_message(
        json::object sub_header,
        json::object sub_msg,
        json::object sub_data
    ) override;
};
