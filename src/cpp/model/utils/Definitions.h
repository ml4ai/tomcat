#pragma once

#include <iostream>
#include <string>

namespace tomcat {
    namespace model {

#define LOG(log) std::cout << log << std::endl
#define LOG_WARNING(log) std::cerr << log << std::endl
#define EXISTS(member, container) (container.find(member) != container.end())

        struct TomcatModelException : public std::exception {
            std::string message;

            TomcatModelException(const std::string& message) : message(message) {}

            const char* what() const throw() { return this->message.data(); }
        };

    } // namespace model
} // namespace tomcat