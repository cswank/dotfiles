{ config, pkgs, lib, ... }:

let
  pkgsUnstable = import <nixpkgs-unstable> {};
in

{
  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.05";

  programs.home-manager = {
    enable = true;
  };

  nixpkgs = {
    config.allowUnfree = true;
  };

  home = {
    username = "craig";
    homeDirectory = "/home/craig";
    packages = [
      pkgsUnstable.tfswitch
      pkgs.nyxt
      pkgs.emacs
      pkgs.keepassxc
      pkgs.guake
      pkgs.silver-searcher
      pkgs.xclip
      pkgs.gh
      pkgs.gcc
      pkgs.gnumake
      pkgs.firefox
      pkgs.git
      pkgs.slack
      pkgs.zoom
      pkgs.zsh
      pkgs.keychain
      pkgs.openvpn
      pkgs.gnupg
      pkgs.mosh
      pkgs.musescore
      pkgs.feh
      pkgs.cifs-utils
      pkgs.ispell
    ];
    file = {
      ".zshrc".source = ./dotfiles/zshrc;
      ".gitignore".source = ./dotfiles/gitignore;
      ".gitconfig".source = ./dotfiles/gitconfig;
    };
  };
}
