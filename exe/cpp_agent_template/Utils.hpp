#pragma once

#include <string>
#include <boost/json.hpp>

using namespace std;
namespace json = boost::json;

/** General use */

class Utils {

    public:

    bool value_matches(json::object obj1, json::object obj2, string key); 

    json::object get_object(string name, json::object source);
    string get_string(string name, json::object source);

    string get_timestamp();
};
