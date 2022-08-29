#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include <boost/json/array.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <set>

#include "Utils.hpp"

using namespace std;
namespace json = boost::json;

// return true if the vector contains the value
bool Utils::contains(const vector<string> haystack, const string needle) {
    for(auto &hay : haystack) {
        if(needle.compare(hay) == 0) {
            return true;
        }
    }
    return false;
}

// return a vector of unique array values for the key
vector<string> Utils::unique_values(const json::array &arr, const string key) {

    set<string> values;
    for(size_t i = 0 ;  i < arr.size() ; i++) {
        json::value element = arr.at(i);
        string value = json::value_to<std::string>(element.at(key));
	if(!value.empty()) {
            values.insert(value);
	}
    }

    vector<string> ret;
    for(auto &value : values) {
        ret.push_back(value);
    }

    return ret;
}

// Report the number of times each key appears in the data vector
void Utils::count_keys(vector<string> data, string report_name){
    json::object obj;
    for(auto &key: data) {
	obj[key] = val<int>(obj, key) + 1;
    }
    cout << report_name << endl;
    for(auto it = obj.begin(); it !=obj.end(); ++it) {
        cout << it->value() << "\t" << it->key()  << endl;
    }
    cout << endl;
    data.clear();
}


/** Return current UTC timestamp in ISO-8601 format. */
string Utils::get_timestamp() {
    return boost::posix_time::to_iso_extended_string(
        boost::posix_time::microsec_clock::universal_time()
    ) + "Z";
}
