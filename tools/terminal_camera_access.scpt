#!/usr/bin/osascript

-- Script to bring the Minecraft window to the front and make it full screen.

on run argv
  set terminal_name to item 1 of argv
  tell application "System Preferences"
    activate

    display dialog "ToMCAT requires enabling microphone access to be able to record audio of the player's face during the mission." buttons {"Cancel", "Continue"} default button "Continue" with icon 1

    reveal anchor "Privacy_Microphone" of pane id "com.apple.preference.security"

    display dialog "Click on the lock in the bottom left of the System Preferences window. You may be prompted for your password. Once the lock is unlocked, click 'Continue'" buttons {"Cancel", "Continue" } default button "Continue" with icon 1

    display dialog "Click on the checkbox next to " & terminal_name & ", and then click 'Later' (you do NOT have to click 'Quit Now')." buttons {"Cancel", "Continue" } default button "Continue" with icon 1

  end tell
end run
