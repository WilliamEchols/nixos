# nixos

NixOS system and applications configuration

# Installation

1. Install NixOS on the target computer (minimal installation with user, disk, and internet settings is sufficient).

Follow `https://nixos.org/manual/nixos/stable/#ch-installation`.

2. Create ssh key and add to Github account.

```bash
ssh-keygen -t rsa -b 4096 -C "your_email@example.com"
```

```bash
cat ~/.ssh/id_ed25519.pub | wl-copy
```

Save to `Github > Settings > SSH and GPG keys`

3. Clone the repo

```bash
git clone https://github.com/williamechols/nixos
```

4. Navigate into the cloned directory

```bash
cd nixos
```

5. Run the automatic setup script

```bash
./setup_nixos
```

6. Write `~/Desktop/nixos/hosts/default.env` for emacs.

Currently, emacs expects two environment variables:
 - `LOCAL_DIRECTORY` contains the path to the org file directory. (i.e., `~/Desktop/orgfiles`)
 - `REMOTE_DIRECTORY` contains the TRAMP route to a remote file system. (i.e., `/ssh:USER@IP#PORT:~/`)

7. Sync emacs org files to `LOCAL_DIRECTORY`.

8. Add ssh key to remote server for emacs TRAMP (if configured).

# Configuration

Nix flake files are located at `~/Desktop/nixos` and system-specific files are located at `~/Desktop/nixos/hosts/default`.

After making modifications, you can test the config with `nixtest` (run configuration but don't save to boot menu) or use the config with `nixit` (run conguration and save to boot menu).

# Recommended file tree from `~/Desktop/`

```
.
├── files
│   ├── dev
│   └── test.txt
├── nixos
│   ├── flake.lock
│   ├── flake.nix
│   ├── hosts
│   ├── README.md
│   └── setup_nixos.sh
├── orgfiles
└── orgfiles-repo
```

In this setup, `files` contains misc/regular files, `nixos` contains this github repo, and `orgfiles` contains the expected path for emacs org files.

# Emacs

Uses elisp to configure `emacs-config.el`, loaded into `emacs.nix`. Also uses `emacs-banner.txt` as ASCII dashboard art. Before updating the NixOS config, eval-buffer can be used to test an updated config.

# Hyprland (display manager)

Hyprland is used as a tiling window manager. Ensure your monitor is defined in `hyprland.conf`, including relative position as well as scaling for best results.

After running `nixtest`/`nixit`, hyprland can be reloaded using `hyprctl reload`.