{ config, pkgs, lib, ... }:

let
  pkgsUnstable = import <unstable> {};

  inherit (lib.generators) toINI;

  greet = pkgs.writeShellScriptBin "greeter" ''
  # Some bash script
    echo Hi
  '';
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

  imports = [ ../files/dconf.nix ];

  programs = {
    home-manager = {
      enable = true;
      path = https://github.com/nix-community/home-manager/archive/release-23.11.tar.gz;
    };

	  gnome-terminal = {
      enable = true;
      showMenubar = false;
      profile = {
         "53d98703-ad0c-4092-850e-3dbea0422cd8" = {
          visibleName = "peninsula";
          default = true;
          #font = "Fira Code Nerd Font";
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

    # firefox = {
    #   enable = true;
    #   profiles = {
    #     myuser = {
    #       id = 0;
    #       settings = {
    #         "media.ffmpeg.vaapi.enabled" = true;
    #         "media.ffvpx.enabled" = false;
    #         "media.av1.enabled" = false;
    #         "gfx.webrender.all" = true;
    #         #"layers.acceleration.force-enabled" = true;
    #       };
    #     };
    #   };
    # };
  };

  nixpkgs = {
    config.allowUnfree = true;
  };

  home = {    
    username = "craig";
    homeDirectory = "/home/craig";
    sessionVariables = {
      MOZ_X11_EGL = "1";
      LIBVA_DRIVER_NAME = "i965";
    };
    packages = [
      pkgsUnstable.tfswitch      
      pkgsUnstable.cryptsetup
      pkgsUnstable.slack
      pkgsUnstable.plantuml
      pkgsUnstable.gron
      pkgsUnstable.winbox
      pkgsUnstable.emacs
      pkgsUnstable.transcribe
      pkgsUnstable.easyrsa
      pkgsUnstable.asciinema
      pkgsUnstable.keepassxc
      pkgsUnstable.terraform-ls
      pkgsUnstable._1password
      pkgsUnstable.texlive.combined.scheme-full
      pkgsUnstable.lilypond-unstable-with-fonts
      pkgsUnstable.simplescreenrecorder
      pkgsUnstable.nodejs
      pkgsUnstable.duckdb
      pkgsUnstable.thrift
      pkgsUnstable.rpi-imager
      pkgsUnstable.kicad
      pkgsUnstable.zig
      pkgsUnstable.zls
      pkgsUnstable.fzf
      pkgsUnstable.go
      pkgsUnstable.jq
      pkgsUnstable.awscli2
      pkgsUnstable.signal-desktop
      pkgsUnstable.acme-sh
      pkgsUnstable.dune3d
      pkgsUnstable.vlc
      pkgsUnstable.ghostty
      #pkgsUnstable.javascript-typescript-langserver
      pkgsUnstable.firefox
      pkgsUnstable.qemu
      pkgsUnstable.quickemu
      pkgsUnstable.picotool
      pkgsUnstable.pico-sdk
      pkgsUnstable.wakelan
      pkgsUnstable.claude-code
      pkgs.nyxt
      pkgs.gnome3.gnome-terminal
      pkgs.gnuplot
      pkgs.zip
      pkgs.unzip
      pkgs.guake
      pkgs.silver-searcher
      pkgs.xclip
      pkgs.gh
      pkgs.gcc
      pkgs.gnumake
      pkgs.git
      pkgs.zsh
      pkgs.keychain
      pkgs.openvpn
      pkgs.gnupg
      pkgs.mosh
      pkgs.screen
      pkgs.musescore
      pkgs.feh
      pkgs.cifs-utils
      pkgs.ispell
      pkgs.shutter
      pkgs.pinentry-curses
      pkgs.graphviz
      pkgs.direnv
      pkgs.lsof
      pkgs.aws-vault
      pkgs.openssl
      pkgs.tree
    ];

    file = {
      ".config/rofi/config.rasi".source = ../files/rofi;
      ".config/i3status/config".source = ../files/i3status;
      ".config/gtk-3.0/settings.ini".source = ../files/gtk.ini;
      ".gnupg/gpg-agent.conf".source = ../files/gpg-agent.conf;
      ".config/mpv/mpv.conf".source = ./files/mpv.conf;
      ".config/ghostty/config".source = ./files/ghostty.conf;
      ".zshrc".source = ../files/zshrc;
      ".gitignore".source = ../files/gitignore;
      ".ssh/config".source = ./files/ssh;
      # ".npmrc".source = ./files/npmrc;   needed to add a github access token
      ".emacs.d" = {
        source = ../files/emacs;
        recursive = true;
      };
    };
  };
}
