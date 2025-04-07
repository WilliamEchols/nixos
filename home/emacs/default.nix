{ pkgs, config, ... }:

{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs29-pgtk;
    extraConfig = builtins.readFile ./init.el;
    extraPackages = epkgs: with epkgs; [
      org
      org-roam
      org-modern
      org-present
      gnu-elpa-keyring-update
      auctex
    ] ++ (with epkgs.melpaStablePackages; [
      ivy
      evil
      dashboard
      general
      org-super-agenda
      which-key
      yasnippet
      company
      company-math
      doom-themes
      doom-modeline
      elfeed
      neotree
      magit
      markdown-mode
      go-mode
      lsp-mode
      lsp-ui
      visual-fill-column
      org-drill
      vertico
      consult
      orderless
      evil-collection
      color-theme-sanityinc-tomorrow
      org-bullets
    ]);
  };
}
