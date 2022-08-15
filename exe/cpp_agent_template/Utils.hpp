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
    bool value_matches(json::object obj1, json::object obj2, string key); 

    json::object get_config(string name, json::object config);

    json::object get_object(string name, json::object source);
    string get_string(string name, json::object source);

    string get_timestamp();
};
