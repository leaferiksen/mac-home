To sync home with this repo, open Terminal as user `leaf` and run
```bash
xcode-select --install
```
```bash
git init && \
git remote add origin git@github.com:leaferiksen/macfiles.git && \
git fetch && \
git checkout -f main
```
To install Homebrew elevate session to `admin` and run
```bash
su - admin
```
```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)" && \
sudo chown -R leaf /opt/homebrew/
```
Spotlight → Full Disk Access → Terminal, then restart Terminal and run
```bash
softwareupdate --install-rosetta --agree-to-license && \
chmod -R +w /opt/homebrew && \
./.config/setup/brew.sh && \
skhd --start-service && \
./.config/setup/mas.sh && \
./.config/setup/preferences.sh && \
./.config/setup/login.sh
```
Download [Karabiner Elements](https://karabiner-elements.pqrs.org/), [LuLu](https://objective-see.org/products/lulu.html) and [LM Studio](https://lmstudio.ai) from their websites

Spotlight → Night Shift options → Schedule → set Sunrise to Sunset

Spotlight → Control Center → Weather → set Show in Menu Bar

[Keyboard](prefs:x-apple.systempreferences:com.apple.Keyboard-Settings.extension) → Language input methods → add Japanese - Romaji