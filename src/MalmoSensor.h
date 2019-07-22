#pragma once

#include <AgentHost.h>

class MalmoSensor {
public:
  MalmoSensor() {}
  MalmoSensor(AgentHost host) {
  }

  void initialize() {};

  void get_observation();
};
