#!/bin/sh

osascript -e '
tell application "System Events"
  set currentApp to name of first process whose frontmost is true
end tell

tell application "Xcode" to activate
tell application "System Events" to keystroke "r" using {command down, shift down}
delay 1
tell application "System Events" to keystroke "r" using {command down}

tell application currentApp to activate
'
