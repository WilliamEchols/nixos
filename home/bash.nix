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
      nixit-e = "sudo nixos-rebuild switch --flake ~/Desktop/nixos/#epsilon";
      nixtest-e = "sudo nixos-rebuild test --flake ~/Desktop/nixos/#epsilon";
      cdnix = "cd ~/Desktop/nixos/";
    };
  };
}
