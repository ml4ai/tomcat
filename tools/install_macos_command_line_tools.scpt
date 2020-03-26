tell application "System Events"
  tell process "Install Command Line Developer Tools"
    keystroke return
    click button "Agree" of window "License Agreement"
  end tell
end tell
