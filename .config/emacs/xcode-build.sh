#!/bin/sh

osascript -e '
tell application "System Events"
  set currentApp to name of first process whose frontmost is true
end tell

tell application "Xcode" to activate
tell application "System Events"
  click menu item "Run" of menu "Product" of menu bar 1 of process "Xcode"
end tell

tell application currentApp to activate
'
