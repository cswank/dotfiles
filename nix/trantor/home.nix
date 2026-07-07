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

    # gnome-terminal is configured via dconf (see ../files/dconf.nix).
    # Do not also use `programs.gnome-terminal` here: both write the same
    # profile keys and home-manager errors out on the conflict.


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

  services.dunst.enable = true;

  systemd.user.services.stomach-reminder = {
    Unit.Description = "Hourly stomach exercise reminder";
    Service = {
      Type = "oneshot";
      ExecStart = "${pkgs.libnotify}/bin/notify-send -u critical 'Stomach exercises' 'Time to do your stomach exercises'";
    };
  };

  systemd.user.timers.stomach-reminder = {
    Unit.Description = "Hourly stomach exercise reminder";
    Timer = {
      OnCalendar = "Mon..Fri 06..14:00:00";
      Persistent = false;
    };
    Install.WantedBy = [ "timers.target" ];
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
      pkgsUnstable._1password-cli
      pkgsUnstable.acme-sh
      pkgsUnstable.asciinema
      pkgsUnstable.awscli2
      pkgsUnstable.claude-code
      pkgsUnstable.cloc
      pkgsUnstable.cryptsetup
      pkgsUnstable.duckdb
      pkgsUnstable.dune3d
      pkgsUnstable.easyrsa
      pkgsUnstable.emacs
      pkgsUnstable.firefox
      pkgsUnstable.fzf
      pkgsUnstable.ghostty
      pkgsUnstable.kitty
      pkgsUnstable.go
      pkgsUnstable.gron
      pkgsUnstable.jq
      pkgsUnstable.keepassxc
      pkgsUnstable.kicad
      pkgsUnstable.lilypond-unstable-with-fonts
      pkgsUnstable.nodejs
      pkgsUnstable.pico-sdk
      pkgsUnstable.picotool
      pkgsUnstable.plantuml
      pkgsUnstable.qemu
      pkgsUnstable.quickemu
      pkgsUnstable.rpi-imager
      pkgsUnstable.signal-desktop
      pkgsUnstable.simplescreenrecorder
      (pkgsUnstable.slack.overrideAttrs (old: {
        postFixup = (old.postFixup or "") + ''
          wrapProgram $out/bin/slack \
            --add-flags "--force-device-scale-factor=2.0"
        '';
      }))
      pkgsUnstable.tenv
      pkgsUnstable.texlive.combined.scheme-full
      pkgsUnstable.thrift
      pkgsUnstable.transcribe
      pkgsUnstable.vlc
      pkgsUnstable.wakelan
      pkgsUnstable.winbox
      pkgsUnstable.zig
      pkgsUnstable.zls
      pkgsUnstable.emacsPackages.vterm
      pkgsUnstable.mermaid-cli
      pkgsUnstable.postgresql_15
      pkgs.aws-vault
      pkgs.cifs-utils
      pkgs.direnv
      pkgs.feh
      pkgs.gcc
      pkgs.libgccjit
      pkgs.gh
      pkgs.git
      pkgs.gnome3.gnome-terminal
      pkgs.gnumake
      pkgs.gnupg
      pkgs.gnuplot
      pkgs.graphviz
      pkgs.guake
      pkgs.ispell
      pkgs.keychain
      pkgs.lsof
      pkgs.mosh
      pkgs.musescore
      pkgs.nyxt
      pkgs.openssl
      pkgs.openvpn
      pkgs.pinentry-curses
      pkgs.screen
      pkgs.shutter
      pkgs.silver-searcher
      pkgs.tree
      pkgs.unzip
      pkgs.xclip
      pkgs.xxd
      pkgs.zip
      pkgs.zsh
    ];

    file = {
      ".config/rofi/config.rasi".source = ../files/rofi;
      ".config/i3status/config".source = ../files/i3status;
      ".config/gtk-3.0/settings.ini".source = ../files/gtk.ini;
      ".gnupg/gpg-agent.conf".source = ../files/gpg-agent.conf;
      ".config/mpv/mpv.conf".source = ./files/mpv.conf;
      ".config/ghostty/config".source = ./files/ghostty.conf;
      ".config/kitty/kitty.conf".source = ./files/kitty.conf;
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
