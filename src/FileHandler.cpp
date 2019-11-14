#include "FileHandler.h"
#include <fstream>
#include <iostream>
#include <sstream>

using namespace std;

namespace tomcat {

  string tomcat::FileHandler::getFileContent(string filename) {
    ifstream inputFile(filename);
    stringstream stringStream;
    stringStream << inputFile.rdbuf();
    inputFile.close();
    return stringStream.str();
  }

} // namespace tomcat
