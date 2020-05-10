#pragma once

#include <string>

namespace tomcat {

    template <class AssociativeContainer, class Value>
    bool in(AssociativeContainer container, Value value) {
        return container.count(value) != 0;
    }

    template <class AssociativeContainer, class Key, class Value>
    Value get(AssociativeContainer container, Key key, Value default_value) {
        return in(container, key) ? container[key] : default_value;
    }

    /** Get timestamp string as <Year>_<Month>_<Day>_<Hour>_<Minute>_<Second>
     * e.g. 2019_01_04_20_26_48 */
    std::string get_timestamp();

} // namespace tomcat
