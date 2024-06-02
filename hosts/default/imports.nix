{ pkgs, config, ... }:

{
  imports = [
    ./waybar.nix
    ./bash.nix
    ./kitty.nix
    ./emacs.nix
    ./lf.nix
  ];
}
