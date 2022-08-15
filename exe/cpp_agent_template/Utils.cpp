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


// return the key value or an empty object if not found
json::object Utils::get_object(string key, json::object source){
    if(source.contains(key)) {
        return json::value_to<json::object>(source.at(key));
    } else {
//       cerr << key << " object not found." << endl;
       return json::object();
    }
}

// return the key value or an empty string if not found
std::string Utils::get_string(string key, json::object source){
    if(source.contains(key)) {
        return json::value_to<std::string>(source.at(key));
    } else {
//       cerr << key << " string not found." << endl;
       return "";
    }
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
