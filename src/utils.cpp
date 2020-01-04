#include "utils.h"
#include <ctime>
#include <iomanip>
#include <sstream>

namespace tomcat {
  std::string get_timestamp() {
    auto now = std::time(nullptr);
    std::ostringstream os;
    os << std::put_time(std::gmtime(&now), "%Y_%m_%d_%H_%M_%S");
    return os.str();
  }
} // namespace tomcat
