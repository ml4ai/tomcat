#include "MissionFactory.h"
#include "FileHandler.h"
#include "Mission.h"
#include "TomcatMission.h"
#include "XMLMission.h"
#include <boost/filesystem.hpp>

using namespace std;
using boost::filesystem::path;

namespace tomcat {

  Mission* MissionFactory::create(string missionIdOrPathToXML) {
    Mission* mission;
    path p(missionIdOrPathToXML);
    if (p.extension() == ".xml") {
      mission = new XMLMission(missionIdOrPathToXML);
    }
    else {
      stringstream missionIdAsString(missionIdOrPathToXML);
      int missionId;
      missionIdAsString >> missionId;

      mission = new TomcatMission(missionId);
    }

    return mission;
  }

} // namespace tomcat
