#pragma once

#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/json/array.hpp>
#include <boost/log/trivial.hpp>

#include <boost/json.hpp>
#include <future>
#include <thread>

namespace json = boost::json;

// general functionality
class Utils {

  public:
    void count_keys(std::vector<std::string> keys);

    // return T value for key or fallback T if key not found
    template <class T>
    T val(const json::object& src, const std::string key, const T fallback) {
        if (src.contains(key)) {
            if (src.at(key) == nullptr) {
                return fallback;
            }
            return json::value_to<T>(src.at(key));
        }
        else {
            return fallback;
        }
    }

    // return T value for key or default T if key not found
    template <class T> T val(const json::object& src, const std::string key) {
        return val(src, key, T());
    }

    std::string get_timestamp();

    std::vector<std::string> get_array_values(const json::array& array,
                                              const std::string key);

    bool contains(const std::vector<std::string> haystack,
                  const std::string needle);
};
