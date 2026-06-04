{ config, pkgs, lib, ... }:

let
  pkgs = import <nixpkgs-unstable> {};

  inherit (lib.generators) toINI;

  greet = pkgs.writeShellScriptBin "greeter" ''
  # Some bash script
    echo Hi
  '';
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
  home.stateVersion = "26.05";

  imports = [ ./dconf.nix ];

  programs = {
    home-manager = {
      enable = true;
    };

    # gnome-terminal is configured via dconf (see ./dconf.nix).
    # Do not also use `programs.gnome-terminal` here: both write the same
    # profile and the profile list gets concatenated, producing a broken
    # duplicate entry that makes the terminal fall back to theme colors
    # (black-on-black).

    firefox = {
      enable = true;
      profiles = {
        myuser = {
          id = 0;
          settings = {
            "media.ffmpeg.vaapi.enabled" = true;
            "media.ffvpx.enabled" = false;
            "media.av1.enabled" = false;
            "gfx.webrender.all" = true;
            "layers.acceleration.force-enabled" = true;
            # Compact toolbar/tabs (0 = normal, 1 = compact, 2 = touch).
            "browser.uidensity" = 1;
            # Cap UI/content scaling instead of the 144-dpi auto value (1.5x),
            # which made the chrome oversized. Raise toward 1.5 if pages are
            # too small, lower toward 1.0 for an even denser UI.
            "layout.css.devPixelsPerPx" = "1.25";
          };
        };
      };
    };
  };

  nixpkgs = {
    config.allowUnfree = true;
  };

  home = {
    username = "craig";
    homeDirectory = "/home/craig";
    packages = [
      pkgs.go
      pkgs.jq
      pkgs.keepassxc
      pkgs.tenv
      pkgs.tfswitch
      pkgs.nyxt
      pkgs.cryptsetup
      pkgs.dconf
      pkgs.gnome-terminal
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
      pkgs.zsh
      pkgs.keychain
      pkgs.openvpn
      pkgs.gnupg
      pkgs.mosh
      pkgs.musescore
      pkgs.feh
      pkgs.cifs-utils
      pkgs.ispell
      pkgs.networkmanager-openvpn
      pkgs.shutter
      pkgs.pinentry-curses
      pkgs.graphviz
    ];

    file = {
      ".config/i3status/config".source = ../files/i3status;
      ".config/gtk-3.0/settings.ini".source = ./files/gtk.ini;
      ".gnupg/gpg-agent.conf".source = ../files/gpg-agent.conf;
      ".config/mpv/mpv.conf".source = ./files/mpv.conf;
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
