-- Script to bring the Minecraft window to the front and make it full screen.

tell application "System Events" to tell application process "java"
  activate
  set value of attribute "AXFullScreen" of window 1 to true
end tell
