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

Settings → Search:

- Night Shift options → Schedule → Sunrise to Sunset ☑
- Control Center → Weather → Show in Menu Bar ☑
- Language input methods → add Japanese - Romaji
- Spoken Content → 
  - System Voice → ⓘ → Voice → Matilda (Premium) 💾
  - System Voice → Matilda (Premium) ☑
- Keyboard Shortcuts →
  - Mission Control & Application windows ☒
  - both Input Sources ☒