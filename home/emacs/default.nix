{ pkgs, config, ... }:

{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs29-pgtk;
    extraConfig = builtins.readFile ./init.el;
    extraPackages = epkgs: with epkgs; [
      org
      org-roam
      org-roam-ui
      org-modern
      org-present
      gnu-elpa-keyring-update
      auctex
      org-fragtog
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
      casual
      undo-fu # tesing (this and down)
      counsel-projectile
      projectile
      prescient
      ivy-prescient
      lsp-pyright
      pyvenv
      typescript-mode
      lua-mode
      json-mode
      vimrc-mode
      cmake-font-lock
      yaml-mode
      format-all
      rainbow-mode
      hl-todo
      gcmh
      ivy-rich
      minions
      avy
      xclip
      evil-terminal-cursor-changer
      emojify
      diff-hl
      blamer
      bind-key
      git-gutter
      git-gutter-fringe
      ranger
      all-the-icons
      evil-commentary
      flycheck
      company-prescient
      company-box
      highlight-escape-sequences
      highlight-numbers
      highlight-symbol
      org-transclusion
      writeroom-mode
      emms
    ]);
  };
}
