{ config, pkgs, ... }:

let
  # use "c" to copy current file's contents
  lfConfig = ''
    map c $cat "$f" | wl-copy
  '';
in
{
  programs.home-manager.enable = true;

  home.packages = with pkgs; [
    lf
    wl-clipboard
    cliphist
  ];

  home.file.".config/lf/lfrc".text = lfConfig;
}
