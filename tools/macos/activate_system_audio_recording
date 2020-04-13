#!/usr/bin/osascript

-- Script to set up system audio recording using BlackHole.
-- Tested using macOS Catalina.

tell application "Audio MIDI Setup"
  activate
end tell

tell application "System Events" to tell application process "Audio MIDI Setup"
  tell splitter group 1 of window "Audio Devices"

    -- Create a multi-output device if one does not already exist.
    if not exists static text "Multi-Output Device" then
      click menu button 1
      tell menu 1 of menu button 1
        click menu item "Create Multi-Output Device"
      end tell
    end if

    -- Configuring the multi-output device
    set allRows to every row of table 1 of scroll area 2
    repeat with r in allRows

      -- Enable all the audio devices in the multi-output device.
      set use_cb to checkbox 1 of UI element 1 of r
      if value of use_cb is 0 then
        click use_cb
      end if

      -- Enabling drift correction
      set drift_correction_cb to checkbox 1 of UI element 3 of r
      if value of drift_correction_cb is 0 then
        click drift_correction_cb
      end if

    end repeat
  end tell
end tell
