#!/bin/sh

osascript -e '
tell application "System Events"
  set currentApp to name of first process whose frontmost is true
end tell

tell application "iPhone Mirroring" to activate

tell application "System Events" to key code 49

tell application "System Events"
  tell process currentApp
    set frontmost to true
  end tell
end tell
'
