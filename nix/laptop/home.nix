{ config, pkgs, ... }:

{
  home = {
    stateVersion = "25.05"; # Please read the comment before changing.
    username = "craigswank";
    homeDirectory = "/Users/craigswank";

    sessionVariables = {
      EDITOR = "emacsclient";
    };

    packages = [
      pkgs.mosh
      pkgs.emacs
      pkgs.keychain
      pkgs.zls
      pkgs.silver-searcher
      pkgs.picotool
      pkgs.go
    ];

    file = {
      # ".config/ghostty/config".source = ./files/ghostty.conf;
      ".zshrc".source = ../files/zshrc;
      ".gitignore".source = ../files/gitignore;
      ".ssh/config".source = ./files/ssh;
      ".emacs.d" = {
        source = ../files/emacs;
        recursive = true;
      };
    };
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
