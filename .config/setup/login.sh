#!/bin/sh

osascript -e 'tell application "System Events" to make login item at end with properties {path:"/Users/leaf/Applications/Maccy.app", hidden:false}'
osascript -e 'tell application "System Events" to make login item at end with properties {path:"/Users/leaf/Applications/Lunar.app", hidden:false}'
osascript -e 'tell application "System Events" to make login item at end with properties {path:"/Users/leaf/Applications/AlDente.app", hidden:false}'
osascript -e 'tell application "System Events" to make login item at end with properties {path:"/Applications/LuLu.app", hidden:false}'
osascript -e 'tell application "System Events" to make login item at end with properties {path:"/Applications/Pure Paste.app", hidden:false}'
osascript -e 'tell application "System Events" to make login item at end with properties {path:"/Users/leaf/Applications/noTunes.app", hidden:false}'
osascript -e 'tell application "System Events" to make login item at end with properties {path:"/Users/leaf/Applications/Ghostty.app", hidden:false}'
osascript -e 'tell application "System Events" to make login item at end with properties {path:"/Users/leaf/Applications/Loop.app", hidden:false}'
osascript -e 'tell application "System Events" to make login item at end with properties {path:"/Applications/Lickable Menu Bar.app", hidden:false}'
osascript -e 'tell application "System Events" to get the name of every login item'
