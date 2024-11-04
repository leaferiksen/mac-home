open Terminal
```bash
xcode-select --install && \
cd ~ && \
git init && \
git remote add origin git@github.com:leaferiksen/macfiles.git && \
git fetch && \
git checkout -f main
```
```bash
su admin
```
```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)" && \
sudo chown -R leaf /opt/homebrew/ && \
sudo cp -R /Users/leaf/Documents/Archive/Colemak-DH/Colemak\ DH.bundle /Library/Keyboard\ Layouts/Colemak\ DH.bundle
```
to enable proxy icons Terminal needs full disk access
System Preferences → Security & Privacy → Full Disk Access
then open a Terminal new window
```bash
./.config/setup/1 Preferences.sh && \
./.config/setup/2 Brew Apps.sh && \
./.config/setup/3 App Store.sh
```
