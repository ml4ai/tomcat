#pragma once

#include <string>
#include <boost/json.hpp>

using namespace std;
namespace json = boost::json;

/** General use */

class Utils {

    public:

    // config file fields 
    struct Configuration {
        string name; 
        string message_type;
        string sub_type;
        string topic;
    };

    // populate a Configuration struct from JSON config 
    bool parse_configuration(
        string name, 
        json::object config, 
        Configuration *c
    );

    bool value_matches(json::object obj, string key, string value); 

    string get_timestamp();
};
