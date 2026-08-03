## Initial user setup

- Accessibility → Vision →
  - Zoom: **☑ Turn on Zoom**
  - Appearance: **↖ Auto**
- Analytics: **☒ Share Mac Analytics with Apple**
- Screen Time: **↖ Set Up Later**
- Siri: **☒ Enable Ask Siri**

### For virtual machines only

```bash
defaults write com.apple.dock "persistent-apps" -array && killall Dock && spctl --global-disable && softwareupdate --install-rosetta --agree-to-license
```

## Sync home with this repo

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

To install Homebrew

``` bash
su admin
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

## Set these by hand

### Night Shift options

- Schedule **▼ Sunrise to Sunset**

### Spotlight

- Results from Apps → **☒ Mail**
- Results from system → **☒  iPhone Apps**
- Search Privacy… → **Bluetooth File Exchange**, **Image Playground**, **Stocks**, **Tips**, **Mail**, **Karabiner-EventViewer, Podcasts, Maps**

### Spoken Content

- System Voice ⓘ → Voice → **↖ Matilda (Premium) 💾**
- System Voice **▼ Matilda (Premium)**

### Language input methods

- \+ → **↖ Japanese - Romaji**

### Keyboard Shortcuts

- Mission Control: **☒ All Desktops**
- Input Sources: **☒ Next & Previous**
- Modifier Keys: **Caps Lock (⇪) key ▼ Escape**

### Wacom Tablet

- Options... → **☒ Show wireless tablet battery status in menu bar**

### Wacom Center

- Mapping → **☑ Show advanced options**
- Mapping → Tablet area → Use custom area... → **11784 by 18144** (6 * screen resolution)
- Osu! custom area → **2700 by 4320** (a fifth of the tablet resolution)

## Commands to set up third party apps

### Ryujinx

`ln -s /Users/leaf/Documents/Games/Ryujinx\ \(Application\ Support\) /Users/leaf/Library/Application\ Support/Ryujinx`

### Adobe Photoshop

```
mkdir /Users/leaf/Library/Preferences/Adobe\ Photoshop\ $(date +%Y)\ Settings/
ln -s /Users/leaf/Documents/Drawing/Photoshop/New\ Doc\ Sizes.json /Users/leaf/Library/Preferences/Adobe\ Photoshop\ $(date +%Y)\ Settings/New\ Doc\ Sizes.json
```
