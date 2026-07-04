#!/bin/sh

osascript -e '
tell application "Xcode" to activate
tell application "System Events"
  click menu item "Run" of menu "Product" of menu bar 1 of process "Xcode"
end tell
'
