#/usr/bin/bash
wmctrl -a 'Minecraft 1.11.2'
# Going to the window with a name containing 'Minecraft'
# using "wmctrl -l" to see the titles of windows
wmctrl -b toggle,fullscreen -r 'Minecraft 1.11.2'
