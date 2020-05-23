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

} // namespace tomcat
