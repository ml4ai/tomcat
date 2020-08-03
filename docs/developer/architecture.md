Architecture
============

ToMCAT extends [Project Malmo](https://github.com/microsoft/malmo) to provide a
platform to study human behavior in a virtual environment, with a focus on
studying human-human and human-machine teaming.

The core of Project Malmo consisted of two components: a static C++ library and
a Java mod for Minecraft. Together, these components exposed an API for
communicating with Minecraft over a TCP socket, thus enabling researchers to
programmatically control avatars in the Minecraft world and declaratively
specify 'missions' that provided suitable environments for reinforcement
learning research.

ToMCAT inverts the original research direction of Project Malmo, placing humans
in the spotlight instead of AI agents. We have vendorized the source of Project
Malmo (external/malmo) and modified it to suit our purposes.
1. The original Java mod (external/malmo/Minecraft) has been extended to
   - allow human control by default
   - add software instrumentation to capture human actions in the Minecraft environment. 
   - add 'missions' (and documentation on how to implement new ones) to support
     developing machine social intelligence.
2. The original C++ code for the Malmo static library has not been changed
   significantly (besides a bit of modernization and code formatting). We use
   this library to create an executable, `runMission` that serves as the driver
   for our experiments.

A significant part of ToMCAT involves a set of robust shell scripts for
installing the software, running single and multiplayer sessions, and uploading
data to servers. We place a heavy premium on automation, and thus eschew
targeting the Windows operating system and non-LAN multiplayer. Our software
is currently tested on macOS and Ubuntu. 
