#include <exception>
#include <fstream>
#include <iostream>
#include <string>
#include <thread>

#include "LocalAgent.h"
#include "WebcamSensor.h"
#include <AgentHost.h>
#include <ClientPool.h>
#include <fmt/core.h>

using fmt::print;
using namespace malmo;
using namespace std;
using namespace std::this_thread;
using namespace std::chrono;

string get_file_contents(string filename) {
  ifstream input_file(filename);
  stringstream sstr;
  sstr << input_file.rdbuf();
  input_file.close();
  return sstr.str();
}

void LocalAgent::set_mission(string mission_xml_filepath) {
  string mission_xml = get_file_contents(mission_xml_filepath);
  this->mission = MissionSpec(mission_xml, true);
  this->mission.requestVideo(640, 480);
  this->mission.timeLimitInSeconds(100);
}

int LocalAgent::startMission(int port=10000) {
  using boost::shared_ptr;
  MissionRecordSpec mission_record;
  WorldState world_state;
  int attempts = 0;
  bool connected = false;

  ClientPool client_pool;
  client_pool.add(ClientInfo("127.0.0.1", port));

  print("Waiting for the mission to start...");
  do {
    try {
      this->host.startMission(this->mission, client_pool, mission_record, 0, "");
      //this->host.startMission(this->mission, mission_record);
      connected = true;
    }
    catch (exception &e) {
      print("Error starting mission: {}", e.what());
      attempts += 1;
      // Give up after three attempts.
      if (attempts >= 3)
        return EXIT_FAILURE;
      else
        // Wait a second and try again.
        sleep_for(milliseconds(1000));
    }
  } while (!connected);

  do {
    sleep_for(milliseconds(100));
    world_state = this->host.getWorldState();
  } while (!world_state.has_mission_begun);

  this->webcam_sensor.initialize();

  do {
    sleep_for(milliseconds(10));
    this->webcam_sensor.get_observation();
    world_state = this->host.getWorldState();
  } while (world_state.is_mission_running);

  print("Mission has stopped.");

  return EXIT_SUCCESS;
}

void LocalAgent::sendCommand(string command) {
  this->host.sendCommand(command);
}
