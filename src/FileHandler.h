#pragma once

#include <string>

namespace tomcat {

  class FileHandler {
  public:
    /**
     * Returns the content of a file
     * @param filename - Path to a file
     * @return
     */
    static std::string getFileContent(std::string filename);
  };

} // namespace tomcat
