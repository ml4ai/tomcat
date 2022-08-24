#pragma once

#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/log/trivial.hpp>
#include <boost/json/array.hpp>

#include <string>
#include <thread>
#include <future>
#include <boost/json.hpp>

namespace json = boost::json;
using namespace std;
using namespace std::chrono;

// general functionality 
class Utils {

    public:

    // return T value for key or fallback T if key not found 
    template <class T>
    T val(const json::object &src,
          const string key,
          const T fallback) {
        if(src.contains(key)) {
	    if(src.at(key) == nullptr) {
                return fallback;
	    }
            return json::value_to<T>(src.at(key));
        } else {
            return fallback;
        }
    }

    // return T value for key or default T if key not found 
    template <class T>
    T val(const json::object &src, const string key) {
        return val(src, key, T());
    }

    string get_timestamp();

    vector<string> unique_values(
        const json::array &array,
        const string key
    );

    bool contains(const vector<string> haystack, const string needle);
};
