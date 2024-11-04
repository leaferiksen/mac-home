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
