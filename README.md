# nixos

NixOS system and applications configuration

# Installation

1. Install NixOS on the target computer (minimal installation with user, disk, and internet settings is sufficient).

Follow `https://nixos.org/manual/nixos/stable/#ch-installation`.

2. Set system git user information

```bash
git config --global user.name "USERNAME"
```

```bash
git config --global user.email "EXAMPLE@MAIL.com"
```

3. Create ssh key and add to Github account.

```bash
ssh-keygen -t rsa -b 4096 -C "your_email@example.com"
```

```bash
cat ~/.ssh/id_rsa.pub | wl-copy
```

Save to `Github > Settings > SSH and GPG keys`

4. Clone the repo

```bash
git clone https://github.com/williamechols/nixos
```

5. Navigate into the cloned directory

```bash
cd nixos
```

6. Run the automatic setup script

```bash
./setup_nixos
```

7. Write `~/Desktop/nixos/home/emacs/.env` for emacs.

Currently, emacs expects the following environment variables:
 - `LOCAL_DIRECTORY` contains the path to the org file directory. (i.e., `~/Desktop/orgfiles`)
 - `REMOTE_DIRECTORY` contains the TRAMP route to a remote file system. (i.e., `/ssh:USER@IP#PORT:~/`)
 - `ERC_NICK` contains the nickname to be used for the emacs IRC client
 - `ERC_NAME` contains the full name for ERC
 - `ERC_PASS` contains the password ERC SASL

8. Sync emacs org files to `LOCAL_DIRECTORY`.

9. Add ssh key to remote server for emacs TRAMP (if configured).

# Configuration

Nix flake files are located at `~/Desktop/nixos` and system-specific files are located at `~/Desktop/nixos/hosts/HOSTNAME`.

After making modifications, you can test the config with `nixtest-l` (lambda desktop) or `nixtest-e` (epsilon laptop) (run configuration but don't save to boot menu) or use the config with `nixit-l` or `nixit-e` (run conguration and save to boot menu).

# Configuration file tree (located in `~/Desktop/`)

```
nixos/
├── flake.lock
├── flake.nix
├── home
│   ├── bash.nix
│   ├── default.nix
│   ├── emacs
│   │   ├── banner.txt
│   │   ├── default.nix
│   │   └── init.el
│   ├── hyprland
│   │   ├── default.nix
│   │   ├── hyprland.conf
│   │   └── hyprland_start.sh
│   ├── kitty.nix
│   ├── lf.nix
│   ├── rofi
│   │   └── rofi-config.rasi
│   ├── wallpapers
│   │   └── default.png
│   └── waybar.nix
├── hosts
│   ├── epsilon
│   │   ├── configuration.nix
│   │   └── hardware-configuration.nix
│   └── lambda
│       ├── configuration.nix
│       └── hardware-configuration.nix
├── modules
│   ├── bluetooth.nix
│   ├── hyprland.nix
│   └── nvidia.nix
├── README.md
└── setup_nixos.sh
```

# Emacs

Before updating the NixOS config, eval-buffer can be used to test an updated config.

# Hyprland (display manager)

Hyprland is used as a tiling window manager. Ensure your monitor is defined in `hyprland.conf`, including relative position as well as scaling for best results.

After compiling the NixOS changes, hyprland can be reloaded using `hyprctl reload`.