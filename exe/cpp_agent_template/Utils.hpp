#pragma once

#include <string>
#include <boost/json.hpp>

using namespace std;
namespace json = boost::json;

/** General use */

class Utils {

    public:

    struct CommonHeader {
        string timestamp;
        string message_type;
        string version;  // testbed version
    };

    struct CommonMsg {
        string experiment_id;
        string timestamp;
        string source;
        string sub_type;
        string version;  // this software version
    };

    struct Configuration {
        string name; 
        string message_type;
        string sub_type;
        string topic;
    };

    bool parse_configuration(
        string name, 
        json::object config, 
        Configuration *c
    );

    bool value_matches(json::object obj, string key, string value); 

    string get_timestamp();
};
