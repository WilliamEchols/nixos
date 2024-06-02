{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.programs.bash;
  cfge = config.environment;
in
{
  programs.bash = {
    enable = true;
    enableCompletion = true;

    shellAliases = {
      nixit = "sudo nixos-rebuild switch --flake ~/Desktop/nixos/#default";
      nixtest = "sudo nixos-rebuild test --flake ~/Desktop/nixos/#default";
      cdnix = "cd ~/Desktop/nixos/hosts/default";
    };
  };
}
