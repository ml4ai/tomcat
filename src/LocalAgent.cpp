#include "LocalAgent.h"
#include "Mission.h"
#include <ClientPool.h>

using fmt::print;
using namespace malmo;
using namespace std;
using namespace std::this_thread;
using namespace std::chrono;

namespace tomcat {

  LocalAgent::LocalAgent() {}
  LocalAgent::~LocalAgent() {}

  void LocalAgent::observe_mission(Mission& mission) {
    // Model will be developed here
  }

} // namespace tomcat
