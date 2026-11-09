 # README.md

## Initial User Setup

- Accessibility → Vision →
  - Zoom: **☑ Turn on Zoom**
  - Appearance: **↖ Auto**
- Analytics: **☒ Share Mac Analytics with Apple**
- Screen Time: **↖ Set Up Later**
- Siri: **☒ Enable Ask Siri**

## Sync Home with This Repo

open Terminal as user `leaf` and run

```bash
xcode-select --install
git init && \
git remote add origin git@github.com:leaferiksen/mac-home.git && \
git fetch && \
git checkout -f main
```

if ssh isn't properly configured with the keychain, try the following before trying again

``` bash
ssh-add --apple-use-keychain ~/.ssh/id_ed25519 &> /dev/null
```

``` bash
su admin
```

``` bash
sudo spctl --master-disable
```

``` bash
sudo chown leaf /opt && exit
```

```bash
git clone https://github.com/Homebrew/brew /opt/homebrew
```

Spotlight → Full Disk Access → /System/Applications/Utilities/Terminal.app, then restart Terminal and run

```bash
softwareupdate --install-rosetta --agree-to-license && \
chmod -R +w /opt/homebrew && \
./.config/setup/apps.sh && \
./.config/setup/preferences.sh && \
./.config/setup/login.sh \
./.config/setup/duti.sh
```

Spotlight → Full Disk Access → remove Terminal

-Download [Karabiner Elements](https://karabiner-elements.pqrs.org/) manually.

## Set These by Hand

### Spotlight

- Results from system → iPhone Apps **☒**
- Clipboard history is available in Spotlight **▼ 7 days**
- Search Privacy… **Bluetooth File Exchange**, **Image Playground**, **Podcasts**, **Stocks**, **Tips**

### Privacy & Security

-Allow applications from **▼ Anywhere**

### Keyboard Shortcuts

- Modifier Keys → Caps Lock (⇪) key **▼ Escape**
