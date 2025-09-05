#!/bin/sh
# open "/System/Library/CoreServices/Menu Extras/Eject.menu"
osascript -e 'tell application "System Events" to make login item at end with properties {path:"/Users/leaf/Applications/AnyBar.app", hidden:false}'
osascript -e 'tell application "System Events" to make login item at end with properties {path:"/Applications/LuLu.app", hidden:false}'
osascript -e 'tell application "System Events" to make login item at end with properties {path:"/Applications/Pure Paste.app", hidden:false}'
osascript -e 'tell application "System Events" to make login item at end with properties {path:"/Users/leaf/Applications/noTunes.app", hidden:false}'
osascript -e 'tell application "System Events" to make login item at end with properties {path:"/Applications/Amphetamine.app", hidden:false}'
osascript -e 'tell application "System Events" to get the name of every login item'
