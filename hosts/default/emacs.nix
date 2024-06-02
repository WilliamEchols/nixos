{ pkgs, config, ... }:

{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs29-pgtk;
    extraConfig = builtins.readFile ./emacs-config.el;
    extraPackages = epkgs: with epkgs; [
      org
      org-roam
      org-roam-ui
      org-bullets
      auctex
    ] ++ (with epkgs.melpaStablePackages; [
      ivy
      evil
      dashboard
      general
      org-super-agenda
      which-key
      company
      company-math
      doom-themes
      doom-modeline
      elfeed
      neotree
      magit
    ]);
  };
}
