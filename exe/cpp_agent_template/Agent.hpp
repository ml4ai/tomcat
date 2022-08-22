#pragma once

#include <boost/json.hpp>
namespace json = boost::json;

using namespace std;


// interface for write method
class Agent {

    public:

    virtual void write(const string topic, json::object &message) = 0;
};
