#!/usr/bin/osascript

-- Script to bring the Minecraft window to the front and make it full screen.

on run argv
  set terminal_name to item 1 of argv
  tell application "System Events"
    set UI_enabled to UI elements enabled
  end tell

  if UI_enabled is false then
    tell application "System Preferences"
      activate
      display dialog "ToMCAT uses assistive access to automatically make Minecraft full screen." buttons {"Cancel", "Continue"} default button "Continue" with icon 1
      activate
      reveal anchor "Privacy_Accessibility" of pane id "com.apple.preference.security"
      display dialog "Click on the lock in the bottom left of the System Preferences window. You will be prompted for your password. Then, click on the checkbox next to " & terminal_name & ", then click on the lock again. Once that is done, click the 'Continue' button." buttons {"Cancel", "Continue" } default button "Continue" with icon 1
    end tell
  end if

  tell application "System Events"
    click UI element "java" of list 1 of application process "Dock"
    tell application process "java"
      if windows is not {} then
        set value of attribute "AXFullScreen" of window "Minecraft 1.11.2" to true
      end if
    end tell
  end tell
end run
