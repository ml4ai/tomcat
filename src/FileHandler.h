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

    /**
     * Returns the file extension from a filename
     * @param filename - Filename with extension (e.g. mission.xml)
     * @return
     */
    static std::string getFileExtensionFromFilename(std::string filename);
  };

} // namespace tomcat
