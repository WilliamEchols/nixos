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
      # delta flake
      nixit-d = "sudo nixos-rebuild switch --flake ~/Desktop/nixos/#delta";
      nixtest-d = "sudo nixos-rebuild test --flake ~/Desktop/nixos/#delta";

      # epsilon flake
      nixit-e = "sudo nixos-rebuild switch --flake ~/Desktop/nixos/#epsilon";
      nixtest-e = "sudo nixos-rebuild test --flake ~/Desktop/nixos/#epsilon";

      # lambda flake
      nixit-l = "sudo nixos-rebuild switch --flake ~/Desktop/nixos/#lambda";
      nixtest-l = "sudo nixos-rebuild test --flake ~/Desktop/nixos/#lambda";

      # theta flake
      nixit-t = "sudo nixos-rebuild switch --flake ~/Desktop/nixos/#theta";
      nixtest-t = "sudo nixos-rebuild test --flake ~/Desktop/nixos/#theta";

      # misc
      cdnix = "cd ~/Desktop/nixos/";
    };
  };
}
