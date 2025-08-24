#!/bin/sh

osascript -e 'tell application "System Preferences" to quit'
# set emacs as default folder handler
defaults write -g "NSFileViewer" -string "org.gnu.Emacs"
# fix scroll direction
defaults write -g "com.apple.swipescrolldirection" -bool "false"
# show scroll bars
defaults write -g "AppleShowScrollBars" -string "Always"
# disable zoom button popup
defaults write -g "NSZoomButtonShowMenu" -bool "false"
# disable controller launchpad
defaults write com.apple.GameController "bluetoothPrefsMenuLongPressAction" -int "0"
# use analogue clock
defaults write com.apple.menuextra.clock "IsAnalog" -bool "true"
##################################################
# Expanded Save/Print Modals and file extensions #
##################################################
defaults write -g "AppleShowAllExtensions" -bool "true"
defaults write -g "NSDocumentSaveNewDocumentsToCloud" -bool "false"
defaults write -g "NSNavPanelExpandedStateForSaveMode" -bool "true"
defaults write -g "NSNavPanelExpandedStateForSaveMode2" -bool "true"
defaults write -g "PMPrintingExpandedStateForPrint" -bool "true"
#################
# accent colors #
#################
defaults write -g "AppleIconAppearanceTheme" -string "TintedAutomatic"
defaults write -g "NSTableViewDefaultSizeMode" -int "3"
defaults write -g "NSColorSimulateHardwareAccent" -bool "yes"
defaults write -g "NSColorSimulatedHardwareEnclosureNumber" -int "4"
defaults write com.apple.universalaccess "showWindowTitlebarIcons" -bool "true"
defaults write com.apple.universalaccess "differentiateWithoutColor" -bool "true"
defaults write com.apple.universalaccess "reduceTransparency" -bool "true"
defaults write com.apple.universalaccess "mouseDriverCursorSize" -float "1.5"
########################
# fuck .DS_Store files #
########################
defaults write com.apple.desktopservices "DSDontWriteUSBStores" -bool "true"
defaults write com.apple.desktopservices "DSDontWriteNetworkStores" -bool "true"
# trackpad cmd+ctrl+three-finger drag
defaults write -g "NSWindowShouldDragOnGesture" -bool "true"
defaults write -g "NSWindowShouldDragOnGestureFeedback" -bool "false"
defaults write com.apple.AppleMultitouchTrackpad "TrackpadThreeFingerDrag" -bool "true"
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad "TrackpadThreeFingerDrag" -bool "true"
defaults write com.apple.AppleMultitouchTrackpad "TrackpadTwoFingerFromRightEdgeSwipeGesture" -int "0"
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad "TrackpadTwoFingerFromRightEdgeSwipeGesture" -int "0"
defaults write com.apple.AppleMultitouchTrackpad "TrackpadFourFingerVertSwipeGesture" -int "0"
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad "TrackpadFourFingerVertSwipeGesture" -int "0"
# disable swiping between desktops
# defaults write com.apple.AppleMultitouchTrackpad "TrackpadFourFingerHorizSwipeGesture" -int "0"
# defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad "TrackpadFourFingerHorizSwipeGesture" -int "0"
# keyboard navigation
defaults write -g "AppleKeyboardUIMode" -int "2"
########################################################
# disable dock, screensaver hot corner and quick notes #
########################################################
defaults write com.apple.dock "autohide" -bool "true"
defaults write com.apple.dock "autohide-delay" -float "1000"
defaults write com.apple.dock "no-bouncing" -bool "true"
defaults write com.apple.dock "persistent-apps" -array
defaults write com.apple.dock "static-only" -bool "true"
defaults write com.apple.dock "tilesize" -int "48"
defaults write com.apple.dock "showhidden" -bool "true"
defaults write com.apple.dock "show-recents" -bool "false"
defaults write com.apple.dock "enable-spring-load-actions-on-all-items" -bool "true"
# don't automatically rearrange Spaces based on most recent use
defaults write com.apple.dock "mru-spaces" -bool "false"
# 0: No Option
# 2: Mission Control
# 3: Show application windows
# 4: Desktop
# 5: Start screen saver
# 6: Disable screen saver
# 7: Dashboard
# 10: Put display to sleep
# 11: Launchpad
# 13: Lock Screen
# 12: Notification Center
defaults write com.apple.dock "wvous-br-corner" -int "1"
# 0: No Modifier
# 131072: Shift Key
# 262144: Control Key
# 524288: Option Key
# 1048576: Command Key
# defaults write com.apple.dock wvous-br-modifier -int "0"
#################
# window tiling #
#################
defaults write com.apple.WindowManager "EnableTilingByEdgeDrag" -int "0"
defaults write com.apple.WindowManager EnableTiledWindowMargins -bool false
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
# Set search scope.
# This Mac       : `SCev`
# Current Folder : `SCcf`
# Previous Scope : `SCsp`
defaults write com.apple.finder "FXDefaultSearchScope" "SCcf"
# Set preferred view style.
# Icon View   : `icnv`
# List View   : `Nlsv`
# Column View : `clmv`
# Cover Flow  : `Flwv`
defaults write com.apple.finder "FXPreferredViewStyle" "clmv"
# find . -name ".DS_Store" -type f -delete
# Set default path for new windows.
# Computer     : `PfCm`
# Volume       : `PfVo`
# $HOME        : `PfHm`
# Desktop      : `PfDe`
# Documents    : `PfDo`
# All My Files : `PfAF`
# Other…       : `PfLo`
defaults write com.apple.finder "NewWindowTarget" "PfDe"
defaults write com.apple.finder "ShowPreviewPane" -bool "true"
/usr/libexec/PlistBuddy -c \
						"add 'PreviewPaneSettings':com.adobe.pdf:showQuickActions bool false" \
						/Users/leaf/Library/Preferences/com.apple.finder.plist
/usr/libexec/PlistBuddy -c \
						"add 'PreviewPaneSettings':com.apple.application:showQuickActions bool false" \
						/Users/leaf/Library/Preferences/com.apple.finder.plist
/usr/libexec/PlistBuddy -c \
						"add 'PreviewPaneSettings':public.archive:showQuickActions bool false" \
						/Users/leaf/Library/Preferences/com.apple.finder.plist
/usr/libexec/PlistBuddy -c \
						"add 'PreviewPaneSettings':public.folder:showQuickActions bool false" \
						/Users/leaf/Library/Preferences/com.apple.finder.plist
/usr/libexec/PlistBuddy -c \
						"add 'PreviewPaneSettings':public.image:showQuickActions bool false" \
						/Users/leaf/Library/Preferences/com.apple.finder.plist
/usr/libexec/PlistBuddy -c \
						"add 'PreviewPaneSettings':public.item:showQuickActions bool false" \
						/Users/leaf/Library/Preferences/com.apple.finder.plist
/usr/libexec/PlistBuddy -c \
						"add 'PreviewPaneSettings':public.movie:showQuickActions bool false" \
						/Users/leaf/Library/Preferences/com.apple.finder.plist
/usr/libexec/PlistBuddy -c \
						"add 'PreviewPaneSettings':public.text:showQuickActions bool false" \
						/Users/leaf/Library/Preferences/com.apple.finder.plist
/usr/libexec/PlistBuddy -c \
						"add 'PreviewPaneSettings':public.html:showQuickActions bool false" \
						/Users/leaf/Library/Preferences/com.apple.finder.plist
defaults write com.apple.finder "CreateDesktop" -bool "false"
defaults write com.apple.finder "QuitMenuItem" -bool "true"
# Disable Notifications
launchctl unload -w /System/Library/LaunchAgents/com.apple.notificationcenterui.plist
# soft restart
killall Finder Dock NotificationCenter
#####################
# Spotlight Privacy #
#####################
defaults write com.apple.assistant.support "Search Queries Data Sharing Status" -int "2"
########
# Mail #
########
# # https://useplaintext.email/
# defaults write com.apple.mail "SendFormat" -string "Plain"
# defaults write com.apple.mail "AutoReplyFormat" -bool "true"
# defaults write com.apple.mail "NumberOfSnippetLines" -int "0"
# # Copy email addresses as `foo@example.com` instead of `Foo Bar <foo@example.com>` in Mail.app
# defaults write com.apple.mail AddressesIncludeNameOnPasteboard -bool false
# # Add the keyboard shortcut ⌘ + Enter to send an email in Mail.app
# defaults write com.apple.mail NSUserKeyEquivalents -dict-add "Send" "@\U21a9"
###########
# noTunes #
###########
defaults write digital.twisted.noTunes replacement '/System/Applications/iPhone Mirroring.app'
########
# IINA #
########
defaults write com.colliderli.iina "currentInputConfigName" -string "Movist Default"
defaults write com.colliderli.iina "initialWindowSizePosition" -string "-0+0"
defaults write com.colliderli.iina "actionAfterLaunch" -int "1"
defaults write com.colliderli.iina "alwaysFloatOnTop" -bool "true"
# defaults write com.colliderli.iina "useLegacyFullScreen" -bool "true"
defaults write com.colliderli.iina "fullScreenWhenOpen" -bool "true"
defaults write com.colliderli.iina "oscPosition" -int "2"
defaults write com.colliderli.iina "arrowBtnAction" -int "1"
defaults write com.colliderli.iina "themeMaterial" -int "4"
defaults write com.colliderli.iina "playlistAutoPlayNext" -bool "false"
defaults write com.colliderli.iina "windowBehaviorWhenPip" -int "1"
defaults write com.colliderli.iina "ytdlSearchPath" -string "/opt/homebrew/opt/"
defaults write com.colliderli.iina "SUEnableAutomaticChecks" -bool "false"
#########################################################
# our lord and savior, Jesus enterprise policies Christ #
#########################################################
defaults write app.zen-browser.zen "EnterprisePoliciesEnabled" -bool "true"
defaults write app.zen-browser.zen "DisableAppUpdate" -bool "true"
############
# Chromium #
############
defaults write org.chromium.Chromium ExtensionManifestV2Availability -int 2
#########
# Hazel #
#########
defaults write com.noodlesoft.Hazel "ShowStatusInMenuBar" -bool "false"
defaults write com.noodlesoft.Hazel "TrashUninstallApps" -bool "true"
defaults write com.noodlesoft.Hazel "SUEnableAutomaticChecks" -bool "false"
##############
# Pure Paste #
##############
defaults write com.sindresorhus.Pure-Paste "NSStatusItem Visible Item-0" -bool "false"
defaults write com.sindresorhus.Pure-Paste "hideMenuBarIcon" -bool "true"
#########
# Maccy #
#########
defaults write org.p0deje.Maccy "showInStatusBar" -bool "false"
defaults write org.p0deje.Maccy "popupPosition" -string "center"
defaults write org.p0deje.Maccy "historySize" -int "10"
defaults write org.p0deje.Maccy "KeyboardShortcuts_popup" -string "{\"carbonModifiers\":768,\"carbonKeyCode\":9}"
defaults write org.p0deje.Maccy "ignoredApps" -array "com.apple.Passwords"
#####################
# Disabled features #
#####################
# defaults write com.apple.Accessibility "ReduceMotionEnabled" -int "1"
# defaults write -g NSScrollViewRubberbanding -int 0
# defaults write com.apple.universalaccess "virtualKeyboardOnOff" -bool "true"
# defaults write com.apple.universalaccess "virtualKeyboardCornerActionType" '{
#     0 = 1;
#     1 = 0;
#     2 = 0;
#     3 = 0;
# }'
# # Use scroll gesture with the Ctrl (^) modifier key to zoom
# defaults write com.apple.universalaccess closeViewScrollWheelToggle -bool true
# # Keep windows when quitting an app
# defaults write -g "NSQuitAlwaysKeepsWindows" -bool "true"
# # Save screenshots to the Pictures/Screenshots
# defaults write com.apple.screencapture location -string "/Users/leaf/Pictures/Screenshots"
# # let karabiner handle media keys
# defaults write -g "com.apple.keyboard.fnState" -bool "true"
# # disable disable globe key,
# defaults write com.apple.HIToolbox "AppleFnUsageType" -int "0"
# # no iphone widgets
# defaults write com.apple.chronod "remoteWidgetsEnabled" -bool "false"
# # monocrome widgets
# defaults write com.apple.widgets "widgetAppearance" -int "0"
# # Universal sidebar toggle
# defaults write -g "NSUserKeyEquivalents" '{
# 	"Hide Sidebar" = "~^$`";
# 	"Show Sidebar" = "~^$`";
# 	"System Settings…" = "~^$,";
# }'
#################
# Stage Manager #
#################
# defaults write com.apple.WindowManager "GloballyEnabled" -bool "true"
# defaults write com.apple.WindowManager "HideDesktop" -bool "false"
# defaults write com.apple.WindowManager "AutoHide" -bool "true"
# defaults write com.apple.WindowManager "AutoHideDelay" -int "0"
##########
# Safari #
##########
# Disable preloading top hit in the background
# defaults write com.apple.Safari PreloadTopHit -bool false
# Disable Quick Website Search
# defaults write com.apple.Safari WebsiteSpecificSearchEnabled -bool false
# Show full URL
# defaults write com.apple.Safari "ShowFullURLInSmartSearchField" -bool "true"
# Enhanced Privacy
# defaults write com.apple.safari "EnableEnhancedPrivacyInRegularBrowsing" -bool "true"
# defaults write com.apple.safari "EnableEnhancedPrivacyInPrivateBrowsing" -bool "true"
# Privacy: don’t send search queries to Apple
# defaults write com.apple.Safari UniversalSearchEnabled -bool false
# defaults write com.apple.Safari SuppressSearchSuggestions -bool true
# disable tracking backdoor
# defaults write com.apple.safari "WebKitPreferences.privateClickMeasurementEnabled" -bool "false"
########
# Loop #
########
# defaults write com.MrKai77.Loop "hideMenuBarIcon" -bool "true"
# defaults write com.MrKai77.Loop "useSystemWindowManagerWhenAvailable" -bool "true"
# /usr/libexec/PlistBuddy -c  "delete :trigger: array '0'" /Users/leaf/Library/Preferences/com.MrKai77.Loop.plist
# /usr/libexec/PlistBuddy -c "add :trigger: integer '59'" /Users/leaf/Library/Preferences/com.MrKai77.Loop.plist
# /usr/libexec/PlistBuddy -c "add :trigger: integer '58'" /Users/leaf/Library/Preferences/com.MrKai77.Loop.plist
# /usr/libexec/PlistBuddy -c "add :trigger: integer '56'" /Users/leaf/Library/Preferences/com.MrKai77.Loop.plist
###############
# Hammerspoon #
###############
# defaults write org.hammerspoon.Hammerspoon MJConfigFile "~/.config/hammerspoon/init.lua"
#####################
# Lickable Menu Bar #
#####################
# defaults write com.ibluebox.aqua-menu-bar "style" -int "3"
# defaults write com.ibluebox.aqua-menu-bar "lows" -bool "false"
###################
# SaneSideButtons #
###################
# defaults write com.janhuelsmann.SaneSideButtons "NSStatusItem Visible Item-0" -bool "false"
###########
# AlDente #
###########
# defaults write com.apphousekitchen.aldente-pro "showDockIcon" -bool "false"
############
# TopNotch #
############
# defaults write pl.maketheweb.TopNotch "hideMenubarIcon" -bool "false"
#########
# Lunar #
#########
# defaults write fyi.lunar.Lunar "hideMenuBarIcon" -bool "true"
