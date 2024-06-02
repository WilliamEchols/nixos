{ config, pkgs, ... }:

{
  imports = [
    ./imports.nix
  ];

  home.username = "pokey";
  home.homeDirectory = "/home/pokey";

  home.stateVersion = "23.11";

  programs.git.enable = true;

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
    # load hyprland config from separate file
    ".config/hypr/hyprland.conf".source = ./hyprland.conf;

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
