# nixos

NixOS system and applications configuration

# Installation

1. Install NixOS on the target computer (minimal installation with user, disk, and internet settings is sufficient).

2. Clone the repo
```bash
git clone https://github.com/williamechols/nixos
```

3. Navigate into the cloned directory
```bash
cd nixos
```

4. Run the automatic setup script
```bash
./setup_nixos
```

# Configuration

Nix flake files are located at `~/Desktop/nixos` and system-specific files are located at `~/Desktop/nixos/hosts/default`.

After making modifications, you can test the config with `nixtest` (run configuration but don't save to boot menu) or use the config with `nixit` (run conguration and save to boot menu).

In particularly, modifications to emacs require `nixtest` or `nixit` before updating the emacs elisp config. 
