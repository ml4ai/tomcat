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

/* return true if the same values of two objects match */
bool Utils::value_matches(json::object obj1, json::object obj2, string key){

    /* test that key exists */
    if(!obj1.contains(key) || !obj2.contains(key)) {
        return false;
    }

    /* test value */
    return (obj1.at(key) == obj2.at(key));
}
