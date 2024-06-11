{ config, pkgs, ... }:

{
  imports = [
    ./hyprland
    ./emacs

    ./waybar.nix
    ./bash.nix
    ./kitty.nix
    ./lf.nix
  ];

  home.username = "pokey";
  home.homeDirectory = "/home/pokey";

  home.stateVersion = "23.11";

  # user home packages
  home.packages = with pkgs; [
    swww
    networkmanagerapplet
    waybar
    dunst
    libnotify
    rofi-wayland
    runelite
  ];

  # external application configurations
  home.file = {
    # set system color theme to dark mode
    ".config/gtk-3.0/settings.ini".text = ''
      [Settings]
      gtk-application-prefer-dark-theme=1
    '';
  };

  home.sessionVariables = {
    NIXOS_OZONE_WL = "1";
  };

  programs.home-manager.enable = true;
}
