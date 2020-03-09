-- Script to bring the Minecraft window to the front and make it full screen.

tell application "System Events"
  click UI element "java" of list 1 of application process "Dock"
  tell application process "java"
    set value of attribute "AXFullScreen" of window 1 to true
  end tell
end tell
