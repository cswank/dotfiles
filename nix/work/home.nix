{ config, pkgs, lib, ... }:

let
  pkgsUnstable = import <nixpkgs-unstable> {};
in {
  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.05";

  programs = {
    home-manager = {
      enable = true;
    };
  };

  nixpkgs = {
    config.allowUnfree = true;
  };

  home = {
    username = "craig";
    homeDirectory = "/home/craig";
    packages = [
      pkgsUnstable.tfswitch
      pkgsUnstable.emacs
      pkgsUnstable.silver-searcher
      pkgsUnstable.keychain
      pkgsUnstable.gnupg
      pkgsUnstable.mosh
      pkgsUnstable.go
      pkgsUnstable.awscli
      pkgsUnstable.postgresql
      pkgsUnstable.jq
    ];

    file = {
      ".gnupg/gpg-agent.conf".source = ../files/gpg-agent.conf;
      ".zshrc".source = ../files/zshrc;
      ".gitignore".source = ../files/gitignore;
      ".ssh/config".source = ./files/ssh;
      ".emacs.d" = {
        source = ../files/emacs;
        recursive = true;
      };
    };
  };
}
