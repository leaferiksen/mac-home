Initial user setup
- Accessibility → Vision →
  - Zoom: **☑ Turn on Zoom**
  - Appearance: **↖ Auto**
- Analytics: **☒ Share Mac Analytics with Apple**
- Screen Time: **↖ Set Up Later**
- Siri: **☒ Enable Ask Siri**

To sync home with this repo, open Terminal as user `leaf` and run
```bash
xcode-select --install
```
```bash
git init && \
git remote add origin git@github.com:leaferiksen/mac-home.git && \
git fetch && \
git checkout -f main
```
To install Homebrew elevate session to `admin` and run
```bash
su - admin
```
```bash
launchctl unload -w /System/Library/LaunchAgents/com.apple.notificationcenterui.plist && \
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)" && \
sudo chown -R leaf /opt/homebrew/
```
Spotlight → Full Disk Access → /System/Applications/Utilities/Terminal.app, then restart Terminal and run
```bash
softwareupdate --install-rosetta --agree-to-license && \
chmod -R +w /opt/homebrew && \
./.config/setup/brew.sh && \
skhd --start-service && \
./.config/setup/mas.sh && \
./.config/setup/preferences.sh && \
./.config/setup/duti.sh && \
./.config/setup/login.sh
```
Spotlight → Full Disk Access → remove Terminal

Download [Karabiner Elements](https://karabiner-elements.pqrs.org/), [LuLu](https://objective-see.org/products/lulu.html) and [LM Studio](https://lmstudio.ai) from their websites

Settings that I haven't (yet) bothered to automate:

`Night Shift options`
Schedule: **▼ Sunrise to Sunset**

`Control Center`
**Weather ▼ Show in Menu Bar**

`Block search of private files`
**Bluetooth File Exchange.app**, **Image Playground.app**, **Stocks.app**, **Tips.app**

`Spoken Content`
System Voice → ⓘ → Voice **💾 Matilda (Premium)**
**System Voice ▼ Matilda (Premium)**

`Language input methods`
➕ → **Japanese - Romaji**

`Keyboard Shortcuts`
Mission Control: **☒ Mission Control** & **☒ Application windows**
Input Sources: **☒ Next & Previous**

`Wacom Tablet`
Options → **☒ Show wireless tablet battery status in menu bar**