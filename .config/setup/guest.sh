#!/bin/sh

# disable dock, screensaver hot corner and quick notes
defaults write com.apple.dock "autohide-delay" -float "1000"
defaults write com.apple.dock "no-bouncing" -bool "true"
defaults write com.apple.dock "persistent-apps" -array
defaults write com.apple.dock "static-only" -bool "true"
defaults write com.apple.dock "autohide" -bool "true"
defaults write com.apple.dock "showhidden" -bool "true"
defaults write com.apple.dock "show-recents" -bool "false"
defaults write com.apple.dock "enable-spring-load-actions-on-all-items" -bool "true"
defaults delete com.apple.dock wvous-br-corner
defaults delete com.apple.dock wvous-br-modifier

killall Dock
