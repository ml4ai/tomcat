#pragma once

#include <string>

namespace tomcat {

  /** Get timestamp string as <Year>_<Month>_<Day>_<Hour>_<Minute>_<Second>
   * e.g. 2019_01_04_20_26_48 */
  std::string get_timestamp();

} // namespace tomcat
