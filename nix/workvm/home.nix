{ config, pkgs, lib, ... }:

let
  pkgsUnstable = import <nixpkgs-unstable> {};

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
  home.stateVersion = "21.05";

  imports = [ ../files/dconf.nix ];

  programs = {
    home-manager = {
      enable = true;
    };

	  gnome-terminal = {
      enable = true;
      showMenubar = false;
      profile = {
        cyplo = {
          visibleName = "peninsula";
          default = true;
          font = "Fira Code Nerd Font";
          showScrollbar = false;
          colors = {
            foregroundColor = "#838394949696";
            backgroundColor = "#00002B2B3636";
            palette = [
              "#070736364242"
              "#DCDC32322F2F"
              "#858599990000"
              "#B5B589890000"
              "#26268B8BD2D2"
              "#D3D336368282"
              "#2A2AA1A19898"
              "#EEEEE8E8D5D5"
              "#00002B2B3636"
              "#CBCB4B4B1616"
              "#58586E6E7575"
              "#65657B7B8383"
              "#838394949696"
              "#6C6C7171C4C4"
              "#9393A1A1A1A1"
              "#FDFDF6F6E3E3"
            ];
          };
        };
      };
    };

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
      pkgsUnstable.tfswitch
      pkgsUnstable.google-chrome
      pkgsUnstable.nyxt
      pkgsUnstable.cryptsetup
      pkgsUnstable.slack
      pkgs.gnome3.dconf
      pkgs.gnome3.gnome-terminal
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
      pkgs.networkmanager_openvpn
      pkgs.shutter
      pkgs.pinentry-curses
      pkgs.graphviz
    ];

    file = {
      ".config/i3status/config".source = ../files/i3status;
      ".config/gtk-3.0/settings.ini".source = ../files/gtk.ini;
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
