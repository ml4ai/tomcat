#pragma once

#include <iostream>
#include <string>

namespace tomcat {
    namespace model {

        // value filled in the data files for time steps where there's no
        // observation for a given node.
#define NO_OBS -1
#define LOG(log) std::cout << log << std::endl
#define LOG_WARNING(log) std::cerr << log << std::endl
#define EXISTS(member, container) (container.find(member) != container.end())
#define EPSILON 10E-16

        struct TomcatModelException : public std::exception {
            std::string message;

            TomcatModelException(const std::string& message)
                : message(message) {}

            const char* what() const throw() { return this->message.data(); }
        };

    } // namespace model
} // namespace tomcat