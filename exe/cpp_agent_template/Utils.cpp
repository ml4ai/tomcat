#include "Utils.hpp"
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/json.hpp>
#include <boost/json/array.hpp>
#include <boost/log/trivial.hpp>
#include <set>

namespace json = boost::json;

// return a vector of unique string values for an array element field
std::vector<std::string> Utils::get_array_values(const json::array& arr,
                                                 const std::string key) {

    std::set<std::string> values;
    for (size_t i = 0; i < arr.size(); i++) {
        json::value element = arr.at(i);
        std::string value = json::value_to<std::string>(element.at(key));
        if (!value.empty()) {
            values.insert(value);
        }
    }
    std::vector<std::string> ret;
    for (auto& value : values) {
        ret.push_back(value);
    }
    return ret;
}

// Report the number of times each key appears in the data vector
void Utils::count_keys(std::vector<std::string> data) {
    json::object obj;
    for (auto& key : data) {
        obj[key] = val<int>(obj, key) + 1;
    }
    for (auto it = obj.begin(); it != obj.end(); ++it) {
        std::cout << it->value() << "\t" << it->key() << std::endl;
    }
    data.clear();
}

/** Return current UTC timestamp in ISO-8601 format. */
std::string Utils::get_timestamp() {
    return boost::posix_time::to_iso_extended_string(
               boost::posix_time::microsec_clock::universal_time()) +
           "Z";
}

