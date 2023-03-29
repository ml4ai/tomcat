#pragma once

#include <stdexcept>

class GeneralException : public std::runtime_error {
  public:
    GeneralException(std::string const& msg) : std::runtime_error(msg) {}
};