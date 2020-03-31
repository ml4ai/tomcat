-- Script to bring the Minecraft window to the front and make it full screen.

tell application "System Events"
  tell application process "java"
    if windows is not {} then
      set value of attribute "AXFullScreen" of window "Minecraft 1.11.2" to true
    end if
  end tell
end tell
