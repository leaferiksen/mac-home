#!/bin/sh

osascript -e 'tell application "System Preferences" to quit'

# fix scroll direction
defaults write -g "com.apple.swipescrolldirection" -bool "false"
# show scroll bars
defaults write -g "AppleShowScrollBars" -string "Always"
# disable zoom button popup
defaults write -g "NSZoomButtonShowMenu" -bool "false"
# accent colors
defaults write -g "NSColorSimulateHardwareAccent" -bool "yes"
defaults write -g "NSColorSimulatedHardwareEnclosureNumber" -int "4"

# Keep windows when quitting an app
# defaults write -g "NSQuitAlwaysKeepsWindows" -bool "true"
# Save screenshots to the Pictures/Screenshots
# defaults write com.apple.screencapture location -string "/Users/leaf/Pictures/Screenshots"

# defaults write com.apple.universalaccess "reduceTransparency" -bool "true"
defaults write com.apple.universalaccess "showWindowTitlebarIcons" -bool "true"
# Use scroll gesture with the Ctrl (^) modifier key to zoom
defaults write com.apple.universalaccess closeViewScrollWheelToggle -bool true

# defaults write com.apple.universalaccess "virtualKeyboardOnOff" -bool "true"
defaults write com.apple.universalaccess "virtualKeyboardCornerActionType" '{
    0 = 1;
    1 = 0;
    2 = 0;
    3 = 0;
}'

# fuck .DS_Store files
defaults write com.apple.desktopservices "DSDontWriteUSBStores" -bool "true"
defaults write com.apple.desktopservices "DSDontWriteNetworkStores" -bool "true"

# trackpad cmd+ctrl+three-finger drag
defaults write -g "NSWindowShouldDragOnGesture" -bool "true"
defaults write -g "NSWindowShouldDragOnGestureFeedback" -bool "false"
defaults write com.apple.AppleMultitouchTrackpad "TrackpadThreeFingerDrag" -bool "true"
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad "TrackpadThreeFingerDrag" -bool "true"
defaults write com.apple.AppleMultitouchTrackpad "TrackpadTwoFingerFromRightEdgeSwipeGesture" -int "0"
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad "TrackpadTwoFingerFromRightEdgeSwipeGesture" -int "0"

# defaults write -g "com.apple.keyboard.fnState" -bool "true" # let karabiner handle media keys
# keyboard: UI control, disable disable globe key, quote/ dash conversion and
defaults write com.apple.HIToolbox "AppleFnUsageType" -int "0"
defaults write -g "AppleKeyboardUIMode" -int "2"
defaults write -g "NSAutomaticQuoteSubstitutionEnabled" -int "false"
defaults write -g "NSAutomaticDashSubstitutionEnabled" -bool "false"

# Hyperkey semi-auto window tiling and sidebar
defaults write -g "NSUserKeyEquivalents" '{
    "Show Sidebar" = "@~^$s";
    "Hide Sidebar" = "@~^$s";
    "Remove Window from Set" = "@~^$r";
}'

###############################################################################

# disable dock, screensaver hot corner and quick notes
defaults write com.apple.dock "autohide-delay" -float "1000"
defaults write com.apple.dock "no-bouncing" -bool "true"
defaults write com.apple.dock "persistent-apps" -array
defaults write com.apple.dock "static-only" -bool "true"
defaults write com.apple.dock "autohide" -bool "true"
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
defaults delete com.apple.dock wvous-br-corner
# 0: No Modifier
# 131072: Shift Key
# 262144: Control Key
# 524288: Option Key
# 1048576: Command Key
defaults delete com.apple.dock wvous-br-modifier

###############################################################################

# Stage Manager
# defaults write com.apple.WindowManager "GloballyEnabled" -bool "true"
defaults write com.apple.WindowManager "HideDesktop" -bool "false"
defaults write com.apple.WindowManager "AutoHide" -bool "true"
defaults write com.apple.WindowManager "AutoHideDelay" -int "0"

###############################################################################

# reduce motion
defaults write com.apple.Accessibility "ReduceMotionEnabled" -int "1"

###############################################################################

# Disable in-app rating requests from apps downloaded from the App Store.
defaults write com.apple.appstore InAppReviewEnabled -int 0

###############################################################################

# no iphone widgets
# defaults write com.apple.chronod "remoteWidgetsEnabled" -bool "false"
# monocrome widgets
defaults write com.apple.widgets "widgetAppearance" -int "0"

###############################################################################

# displays don't have separate Spaces (breaks shit)
# defaults write com.apple.spaces.plist spans-displays -bool true

###############################################################################

# Finder
chflags nohidden ~/Library
# defaults write com.apple.finder "CreateDesktop" -bool "false"
# defaults write com.apple.finder "QuitMenuItem" -bool "true"
defaults write com.apple.finder "FXPreferredViewStyle" -string "clmv"
defaults write com.apple.finder "ShowPathbar" -bool "true"
defaults write com.apple.finder "_FXSortFoldersFirst" -bool "true"
defaults write -g "AppleShowAllExtensions" -bool "true"
defaults write com.apple.finder "FXEnableExtensionsChangeWarning" -bool "false"
defaults write -g "com.apple.springing.delay" -float "0.35"
defaults write -g "NSDocumentSaveNewDocumentsToCloud" -bool "false"
defaults write -g "NSNavPanelExpandedStateForSaveMode" -bool "true"
defaults write -g "NSNavPanelExpandedStateForSaveMode2" -bool "true"
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
defaults write com.apple.finder "NewWindowTarget" "PfHm"
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

###############################################################################

# Safari
# Disable AutoFill
defaults write com.apple.Safari "AutoFillPasswords" -bool "false"
# Show full URL
defaults write com.apple.Safari "ShowFullURLInSmartSearchField" -bool "true"
# Enhanced Privacy
defaults write com.apple.safari "EnableEnhancedPrivacyInRegularBrowsing" -bool "true"
defaults write com.apple.safari "EnableEnhancedPrivacyInPrivateBrowsing" -bool "true"
# Privacy: don’t send search queries to Apple
defaults write com.apple.Safari UniversalSearchEnabled -bool false
defaults write com.apple.Safari SuppressSearchSuggestions -bool true
# disable tracking backdoor
defaults write com.apple.safari "WebKitPreferences.privateClickMeasurementEnabled" -bool "false"
# ask where to download
defaults write com.apple.Safari.SandboxBroker "AlwaysPromptForDownloadFolder" -bool "true"
# develop menu and the web inspector
defaults write com.apple.Safari "IncludeDevelopMenu" -bool "true"
defaults write com.apple.Safari "DeveloperMenuVisibility" -bool "true"
defaults write com.apple.Safari "WebKitDeveloperExtrasEnabledPreferenceKey" -bool "true"
defaults write com.apple.Safari "WebKitPreferences.developerExtrasEnabled" -bool "true"
defaults write com.apple.Safari.SandboxBroker "ShowDevelopMenu" -bool "true"
# Set up UserScripts
cp '/Users/leaf/Documents/Archive/browser stuff/Fix Google Search.js' '/Users/leaf/Library/Containers/com.userscripts.macos.Userscripts-Extension/Data/Documents/scripts/Fix Google Search.js'

###############################################################################

# Mail
defaults write com.apple.mail "SendFormat" -string "Plain"
defaults write com.apple.mail "NSUserKeyEquivalents" '{
    Send = "@\\U21a9";
}'

###############################################################################

# Terminal
# open "/Users/leaf/.config/setup/Basic Large.terminal"
# defaults write com.apple.Terminal "Default Window Settings" -string "Basic Large"
# defaults write com.apple.Terminal "Startup Window Settings" -string "Basic Large"
defaults write com.apple.Terminal ShowLineMarks -int 0

###############################################################################

# IINA
defaults write com.colliderli.iina "currentInputConfigName" -string "VLC Default"
defaults write com.colliderli.iina "actionAfterLaunch" -int "1"
defaults write com.colliderli.iina "oscPosition" -int "2"
defaults write com.colliderli.iina "arrowBtnAction" -int "1"
defaults write com.colliderli.iina "themeMaterial" -int "4"
defaults write com.colliderli.iina "windowBehaviorWhenPip" -int "1"
defaults write com.colliderli.iina "ytdlSearchPath" -string "/opt/homebrew/opt/"
defaults write com.colliderli.iina "SUEnableAutomaticChecks" -bool "false"

###############################################################################

# Loop
defaults write com.MrKai77.Loop "hideMenuBarIcon" -bool "true"
defaults write com.MrKai77.Loop "useSystemWindowManagerWhenAvailable" -bool "true"
/usr/libexec/PlistBuddy -c  "delete :trigger:0" \
/Users/leaf/Library/Preferences/com.MrKai77.Loop.plist
/usr/libexec/PlistBuddy -c "add :trigger: integer '55'" \
/Users/leaf/Library/Preferences/com.MrKai77.Loop.plist
/usr/libexec/PlistBuddy -c "add :trigger: integer '58'" \
/Users/leaf/Library/Preferences/com.MrKai77.Loop.plist
/usr/libexec/PlistBuddy -c "add :trigger: integer '59'" \
/Users/leaf/Library/Preferences/com.MrKai77.Loop.plist
/usr/libexec/PlistBuddy -c "add :trigger: integer '56'" \
/Users/leaf/Library/Preferences/com.MrKai77.Loop.plist

###############################################################################

# our lord and savior, Jesus enterprise policies Christ
defaults write org.mozilla.thunderbird "EnterprisePoliciesEnabled" -bool "true"
defaults write org.mozilla.thunderbird "DisableAppUpdate" -bool "true"

###############################################################################

# AlDente
defaults write com.apphousekitchen.aldente-pro "showDockIcon" -bool "false"

###############################################################################

# Lunar
defaults write fyi.lunar.Lunar "hideMenuBarIcon" -bool "true"

###############################################################################

# Hazel
defaults write com.noodlesoft.Hazel "ShowStatusInMenuBar" -bool "false"
defaults write com.noodlesoft.Hazel "TrashUninstallApps" -bool "true"
defaults write com.noodlesoft.Hazel "SUEnableAutomaticChecks" -bool "false"

###############################################################################

# Pure Paste
defaults write com.sindresorhus.Pure-Paste "NSStatusItem Visible Item-0" -bool "false"

###############################################################################

# Maccy
defaults write read org.p0deje.Maccy "KeyboardShortcuts_popup" -string "{\"carbonKeyCode\":9,\"carbonModifiers\":6912}"
defaults write org.p0deje.Maccy "showInStatusBar" -bool "false"
defaults write org.p0deje.Maccy "popupPosition" -string "center"

###############################################################################

# Lickable Menu Bar
# defaults write com.ibluebox.aqua-menu-bar "style" -int "3"
defaults write com.ibluebox.aqua-menu-bar "lows" -bool "false"

###############################################################################

# SaneSideButtons
defaults write com.janhuelsmann.SaneSideButtons "NSStatusItem Visible Item-0" -bool "false"

###############################################################################

# TopNotch
# defaults write pl.maketheweb.TopNotch "hideMenubarIcon" -bool "false"

###############################################################################

# login items
osascript -e 'tell application "System Events" to make login item at end with properties {path:"/Users/leaf/Applications/Maccy.app", hidden:false}'
osascript -e 'tell application "System Events" to make login item at end with properties {path:"/Users/leaf/Applications/Lunar.app", hidden:false}'
osascript -e 'tell application "System Events" to make login item at end with properties {path:"/Users/leaf/Applications/AlDente.app", hidden:false}'
osascript -e 'tell application "System Events" to make login item at end with properties {path:"/Applications/LuLu.app", hidden:false}'
osascript -e 'tell application "System Events" to make login item at end with properties {path:"/Applications/Pure Paste.app", hidden:false}'
osascript -e 'tell application "System Events" to make login item at end with properties {path:"/Users/leaf/Applications/noTunes.app", hidden:false}'
osascript -e 'tell application "System Events" to make login item at end with properties {path:"/Users/leaf/Applications/SaneSideButtons.app", hidden:false}'
osascript -e 'tell application "System Events" to make login item at end with properties {path:"/Users/leaf/Applications/Hyperkey.app", hidden:false}'
osascript -e 'tell application "System Events" to make login item at end with properties {path:"/Users/leaf/Applications/Loop.app", hidden:false}'
osascript -e 'tell application "System Events" to make login item at end with properties {path:"/Applications/Lickable Menu Bar.app", hidden:false}'
osascript -e 'tell application "System Events" to get the name of every login item'

###############################################################################

# soft restart
killall Finder Dock