#!/bin/sh

osascript -e 'tell application "System Preferences" to quit'
# set emacs as default folder handler
# defaults write -g "NSFileViewer" -string "org.gnu.Emacs"
# fix scroll direction
defaults write -g "com.apple.swipescrolldirection" -bool "false"
# show scroll bars
# defaults write -g "AppleShowScrollBars" -string "Always"
# disable zoom button popup
defaults write -g "NSZoomButtonShowMenu" -bool "false"
# Expanded Save/Print Modals and file extensions
defaults write -g "AppleShowAllExtensions" -bool "true"
defaults write -g "NSDocumentSaveNewDocumentsToCloud" -bool "false"
defaults write -g "NSNavPanelExpandedStateForSaveMode" -bool "true"
defaults write -g "NSNavPanelExpandedStateForSaveMode2" -bool "true"
defaults write -g "PMPrintingExpandedStateForPrint" -bool "true"
defaults write com.apple.universalaccess "mouseDriverCursorSize" -float "1.5"
defaults write com.apple.universalaccess "showWindowTitlebarIcons" -bool "true"
# defaults write com.apple.universalaccess "stickyKey" -bool "true"
# defaults write com.apple.universalaccess "stickyKeyBeepOnModifier" -bool "false"
# defaults write com.apple.universalaccess "useStickyKeysShortcutKeys" -bool "true"
# defaults write com.apple.universalaccess "stickyKeysLocation" -int "1"
# less .DS_Store files
defaults write com.apple.desktopservices "DSDontWriteUSBStores" -bool "true"
defaults write com.apple.desktopservices "DSDontWriteNetworkStores" -bool "true"
# trackpad cmd+ctrl+three-finger drag
# defaults write -g "NSWindowShouldDragOnGesture" -bool "true"
# defaults write -g "NSWindowShouldDragOnGestureFeedback" -bool "false"
# defaults write com.apple.AppleMultitouchTrackpad "TrackpadThreeFingerDrag" -bool "true"
# defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad "TrackpadThreeFingerDrag" -bool "true"
# Keep this part for paneru even when not using three finger drag
defaults write com.apple.AppleMultitouchTrackpad "TrackpadTwoFingerFromRightEdgeSwipeGesture" -int "0"
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad "TrackpadTwoFingerFromRightEdgeSwipeGesture" -int "0"
defaults write com.apple.AppleMultitouchTrackpad "TrackpadFourFingerVertSwipeGesture" -int "2"
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad "TrackpadFourFingerVertSwipeGesture" -int "2"
# disable swiping between desktops
defaults write com.apple.AppleMultitouchTrackpad "TrackpadFourFingerHorizSwipeGesture" -int "0"
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad "TrackpadFourFingerHorizSwipeGesture" -int "0"
# keyboard navigation
defaults write -g "AppleKeyboardUIMode" -int "2"
# Use scroll gesture with the Ctrl (^) modifier key to zoom
defaults write com.apple.universalaccess closeViewScrollWheelToggle -bool true
# speed up key-repeat
defaults write -g InitialKeyRepeat -int 15
defaults write -g KeyRepeat -int 2
defaults write -g "ApplePressAndHoldEnabled" -bool "false"
# disable the Control-Command-D binding
defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add 70 '<dict><key>enabled</key><false/></dict>'
# disable input source switching (60 = previous, 61 = next)
for id in 60 61; do
  defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add $id '<dict><key>enabled</key><false/></dict>'
done
# disable "Switch to Desktop N" shortcuts (118-127 = Desktop 1-10)
for id in 118 119 120 121 122 123 124 125 126 127; do
  defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add $id '<dict><key>enabled</key><false/></dict>'
done
# apply shortcut changes without logging out (may be a no-op on newer macOS)
/System/Library/PrivateFrameworks/SystemAdministration.framework/Resources/activateSettings -u 2>/dev/null || true
# disable single-click globe key action (it breaks custom actions in karabiner)
defaults write com.apple.HIToolbox "AppleFnUsageType" -int "0"
# disable dock, screensaver hot corner and quick notes
defaults write com.apple.dock "autohide" -bool "true"
defaults write com.apple.dock "autohide-delay" -float "1000"
defaults write com.apple.dock "enable-spring-load-actions-on-all-items" -bool "true"
defaults write com.apple.dock "no-bouncing" -bool "true"
defaults write com.apple.dock "persistent-apps" -array
defaults write com.apple.dock "show-recents" -bool "false"
defaults write com.apple.dock "showhidden" -bool "true"
defaults write com.apple.dock "static-only" -bool "true"
defaults write com.apple.dock "tilesize" -int "48"
# don't automatically rearrange Spaces based on most recent use
defaults write com.apple.dock "mru-spaces" -bool "false"
#################
# window tiling #
#################
defaults write -g "AppleActionOnDoubleClick" -string "Fill"
defaults write com.apple.WindowManager "EnableTilingByEdgeDrag" -bool "false"
defaults write com.apple.WindowManager "EnableTopTilingByEdgeDrag" -bool "false"
# defaults write com.apple.WindowManager "EnableTiledWindowMargins" -bool "false"
###############
# Lock Screen #
###############
defaults write com.apple.loginwindow ClockFontIdentifier -string "slab"
defaults write com.apple.loginwindow ClockFontWeight -float "800"
#############
# App Store #
#############
defaults write com.apple.appstore "InAppReviewEnabled" -int "0"
##########
# Finder #
##########
chflags nohidden ~/Library
defaults write com.apple.finder "FXPreferredViewStyle" -string "clmv"
defaults write com.apple.finder "ShowPathbar" -bool "true"
defaults write com.apple.finder "_FXSortFoldersFirst" -bool "true"
defaults write com.apple.finder "SidebarTagsSctionDisclosedState" -bool "false"
defaults write com.apple.finder "FXEnableExtensionsChangeWarning" -bool "false"
defaults write com.apple.finder "CreateDesktop" -bool "false"
# soft restart
killall Finder Dock NotificationCenter
##############
# Night Shift #
##############
# needs `brew install smudge/smudge/nightlight` (add to apps.sh)
if command -v nightlight >/dev/null 2>&1; then
  # sunrise to sunset
  nightlight schedule start
fi
#####################
# Spotlight Privacy #
#####################
defaults write com.apple.assistant.support "Search Queries Data Sharing Status" -int "2"
########
# Mail #
########
# https://useplaintext.email/
defaults write -app Mail "SendFormat" -string "Plain"
defaults write -app Mail "AutoReplyFormat" -bool "true"
defaults write -app Mail "NumberOfSnippetLines" -int "0"
# Copy email addresses as `foo@example.com` instead of `Foo Bar <foo@example.com>` in Mail.app
defaults write -app Mail AddressesIncludeNameOnPasteboard -bool false
# Add the keyboard shortcut ⌘ + Enter to send an email in Mail.app
defaults write -app Mail NSUserKeyEquivalents -dict-add "Send" "@\U21a9"
###############
# Hammerspoon #
###############
defaults write org.hammerspoon.Hammerspoon MJConfigFile "~/.config/hammerspoon/init.lua"
###########
# Homerow #
###########
defaults write com.superultra.Homerow "check-for-updates-automatically" -bool "false"
defaults write com.superultra.Homerow "is-scroll-shortcuts-enabled" -bool "false"
defaults write com.superultra.Homerow "label-characters" -string "arstgmneio"
defaults write com.superultra.Homerow "label-font-size" -float "12"
defaults write com.superultra.Homerow "non-search-shortcut" -string "F19"
defaults write com.superultra.Homerow "scroll-keys" -string "bnpf"
defaults write com.superultra.Homerow "scroll-shortcut" -string "F20"
defaults write com.superultra.Homerow "search-shortcut" -string ""
##############
# Pure Paste #
##############
defaults write com.sindresorhus.Pure-Paste "NSStatusItem Visible Item-0" -bool "false"
defaults write com.sindresorhus.Pure-Paste "hideMenuBarIcon" -bool "true"
########
# IINA #
########
# defaults write com.colliderli.iina "fullScreenWhenOpen" -bool "true"
# defaults write com.colliderli.iina "initialWindowSizePosition" -string "-0+0"
# defaults write com.colliderli.iina "useLegacyFullScreen" -bool "true"
# defaults write com.colliderli.iina "windowBehaviorWhenPip" -int "1"
defaults write com.colliderli.iina "SUEnableAutomaticChecks" -bool "false"
defaults write com.colliderli.iina "alwaysFloatOnTop" -bool "true"
defaults write com.colliderli.iina "arrowBtnAction" -int "1"
defaults write com.colliderli.iina "currentInputConfigName" -string "mpv Default"
defaults write com.colliderli.iina "disablePlaySliderScrolling" -bool "true"
defaults write com.colliderli.iina "disableVolumeSliderScrolling" -bool "true"
defaults write com.colliderli.iina "fullScreenWhenOpen" -bool "true"
defaults write com.colliderli.iina "oscPosition" -int "2"
defaults write com.colliderli.iina "playlistAutoPlayNext" -bool "false"
defaults write com.colliderli.iina "scaleRemainingTime" -bool "true"
defaults write com.colliderli.iina "showChapterPos" -bool "true"
defaults write com.colliderli.iina "showRemainingTime" -bool "true"
defaults write com.colliderli.iina "themeMaterial" -int "4"
