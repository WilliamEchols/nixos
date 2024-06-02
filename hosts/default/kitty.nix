{ pkgs, config, ... }:

{
  programs.kitty = {
    enable = true;
    package = pkgs.kitty;
    theme = "Space Gray";
    font.name = "JetBrains Mono";
    font.size = 12;
    settings = {
      scrollback_lines = 2000;
      wheel_scroll_min_lines = 1;
      window_padding_width = 6;
      confirm_os_window_close = 0;
      background_opacity = "0.75";
    };
    extraConfig = ''
      tab_bar_style fade
      tab_fade 1
      active_tab_font_style   bold
      inactive_tab_font_style bold
    '';
  };
}
