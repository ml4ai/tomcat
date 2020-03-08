-- Script to get the window size and position of the Minecraft window on macOS.

tell application "System Events" to tell application process "java"
  set windowposition to position of window 1
  set windowsize to size of window 1
end tell

windowposition & windowsize
