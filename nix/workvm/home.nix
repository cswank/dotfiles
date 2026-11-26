{ config, pkgs, lib, ... }:

let
  pkgs = import <nixpkgs-unstable> { config.allowUnfree = true; };

  # Same revision as trantor's flake.lock. workvm uses channels, not a flake,
  # so it's pinned here instead.
  sopsNix = builtins.fetchTarball {
    url = "https://github.com/Mic92/sops-nix/archive/d5abe5e53200ad48254e8e6bd95e6cef94e66577.tar.gz";
    sha256 = "0ih5smbbw9b0qkx2169xqaq7brdbx7xayj5zcwv7aq0df713z7ys";
  };

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

  imports = [ ./dconf.nix "${sopsNix}/modules/home-manager/sops.nix" ];

  # Secrets are encrypted to this machine's age key and trantor's (see
  # ../.sops.yaml); edit one with `sops nix/secrets/<name>`. A systemd user
  # service decrypts them into $XDG_RUNTIME_DIR and symlinks each to its
  # `path`, so no plaintext reaches the nix store.
  sops = {
    age.keyFile = "${config.home.homeDirectory}/.config/sops/age/keys.txt";
    secrets.pgpass = {
      sopsFile = ../secrets/pgpass;
      format = "binary";
      path = "${config.home.homeDirectory}/.pgpass";
    };
  };

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

  home = {
    username = "craig";
    homeDirectory = "/home/craig";
    packages = [
      pkgs.awscli2
      pkgs.unzip
      pkgs.zip
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
      pkgs.claude-code
      pkgs.keepassxc
      pkgs.guake
      pkgs.silver-searcher-ng
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
      pkgs.shutter
      pkgs.pinentry-curses
      pkgs.graphviz
      pkgs.emacsPackages.vterm
      pkgs.postgresql_18
      pkgs.yarn
      pkgs.nodejs
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
