// --------------------------------------------------------------------------------------------------
//  Copyright (c) 2016 Microsoft Corporation
//
//  Permission is hereby granted, free of charge, to any person obtaining a copy
//  of this software and associated documentation files (the "Software"), to
//  deal in the Software without restriction, including without limitation the
//  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
//  sell copies of the Software, and to permit persons to whom the Software is
//  furnished to do so, subject to the following conditions:
//
//  The above copyright notice and this permission notice shall be included in
//  all copies or substantial portions of the Software.
//
//  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
//  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
//  IN THE SOFTWARE.
// --------------------------------------------------------------------------------------------------

// Local:
#include "FindSchemaFile.h"

// STL:
#include <boost/filesystem.hpp>
#include <fstream>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <string>

using namespace std;
namespace fs = boost::filesystem;
using fs::path, fs::exists;

namespace malmo {
    string FindSchemaFile(const string& name) {
        char* tomcat_dir = getenv("TOMCAT");
        if (tomcat_dir == NULL) {
            throw runtime_error(
                "The TOMCAT environment variable has not been set."
                " Please set it to point to your local copy of the "
                "ToMCAT repository");
        }
        path schema_path = path(tomcat_dir) / "external/malmo/Schemas" / name;
        if (!exists(schema_path)) {
            throw runtime_error("Schema file " + schema_path.string() +
                                "not found!");
        }
        return schema_path.string();
    }
} // namespace malmo
