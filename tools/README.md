README
------

This directory contains:
1. Tools for automating the installation of tomcat and its dependencies.
2. Developer tools and scripts - essentially a sandbox to test out new things.

local-ports
-----------

The `local-ports` directory contains custom Portfiles to install dependencies
for MacPorts users in the following cases.

1. The upstream MacPorts tree does not contain a dependency that is required by
   tomcat. Ideally, in this scenario, a tomcat developer will submit a pull
   request to the MacPorts upstream repository with that dependency.
2. The upstream MacPorts tree has a version of a dependency that is not
   compatible with tomcat or one of its dependencies. This is currently the
   case (as of 2020/02/23) - Malmo is not compatible with `Java 1.8.0_242`, so we
   have added a modified Portfile to get Java `1.8.0_232` instead.
