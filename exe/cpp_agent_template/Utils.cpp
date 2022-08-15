#include <boost/date_time/posix_time/posix_time.hpp>
#include "Utils.hpp"
#include <boost/json.hpp>
#include <boost/log/trivial.hpp>

using namespace std;
namespace json = boost::json;

/** Get current UTC timestamp in ISO-8601 format. */
string Utils::get_timestamp() {
    return boost::posix_time::to_iso_extended_string(
               boost::posix_time::microsec_clock::universal_time()) +
           "Z";
}


// return true if all the configuration fields were found 
bool Utils::parse_configuration(
    string name,
    json::object config,
    Configuration *c
) {
    if(name.empty()) {  // empty name means skip this configuration
        return true;
    }
    if(!config.contains(name)) {
        return false;
    }
    json::object info = json::value_to<json::object>(config.at(name));
    if(!info.contains("topic") ||
       !info.contains("message_type") || 
       !info.contains("sub_type") 
    ) {
        return false;
    }
    c->topic = json::value_to<string>(info.at("topic"));
    c->message_type = json::value_to<string>(info.at("message_type"));
    c->sub_type = json::value_to<string>(info.at("sub_type"));
    c->name = name;
    return true;
}

// return the configuration element for the given name. 
// Showtopper if not found
json::object Utils::get_config(string name, json::object config){
    if(config.contains(name)) {
        return json::value_to<json::object>(config.at(name));
    } else {
       cerr << name << " configuration not found" << endl;
       exit(EXIT_FAILURE);
    }
}

// return the named object if it is found in the source otherwise
// return an empty object
json::object Utils::get_object(string name, json::object source){
    if(source.contains(name)) {
        return json::value_to<json::object>(source.at(name));
    } else {
       cerr << name << " object not found." << endl;
       return json::object();
    }
}

// return the configuration element for the given name. 
// Showtopper if not found
std::string Utils::get_string(string name, json::object source){
    if(source.contains(name)) {
        return json::value_to<std::string>(source.at(name));
    } else {
       cerr << name << " string not found." << endl;
       return "";
    }
}

/* return true if object value matches input */
bool Utils::value_matches(json::object obj, string key, string value) {
    /* test that key exists */
    if(!obj.contains(key)) {
        return false;
    }

    /* test value */
    string keyval = json::value_to<string>(obj.at(key));

    return (value.compare(keyval) == 0);
}

/* return true if object value matches input */
bool Utils::value_matches(json::object obj1, json::object obj2, string key){
    /* test that key exists */
    if(!obj1.contains(key)) {
        return false;
    }
    /* test that key exists */
    if(!obj2.contains(key)) {
        return false;
    }

    /* test value */
    string val1 = json::value_to<string>(obj1.at(key));
    string val2 = json::value_to<string>(obj2.at(key));

    return (val1.compare(val2) == 0);
}
