{ config, pkgs, lib, inputs, ... }:

let
  # Temporary override: pin claude-code to 2.1.280 (first release with Opus 5.5).
  # nixpkgs-unstable is still on 2.1.278 as of 2026-09-23; drop this overlay and
  # the manifest file once the channel catches up to >= 2.1.280.
  #
  # The nixpkgs package takes `manifest` as a top-level arg and derives version +
  # src (url + sha256) entirely from it, so `.override` with a newer manifest is
  # all that's needed -- the manifest's per-platform `checksum` is the fetchurl
  # hash, so no separate prefetch is required.
  claudeCodeOverlay = final: prev: {
    claude-code = prev.claude-code.override {
      manifest = builtins.fromJSON (builtins.readFile ./files/claude-code-manifest.json);
    };
  };

  # Temporary override: rpi-imager 2.0.12 (released 2026-10-08). nixpkgs is
  # still on 2.0.11.1 as of 2026-10-26; drop this overlay once it catches up.
  # The package's fake-git and version strings derive from finalAttrs.version,
  # so bumping version + src is enough.
  rpiImagerOverlay = final: prev: {
    rpi-imager = prev.rpi-imager.overrideAttrs (finalAttrs: old: {
      version = "2.0.12";
      src = prev.fetchFromGitHub {
        owner = "raspberrypi";
        repo = "rpi-imager";
        tag = "v${finalAttrs.version}";
        hash = "sha256-oIq7+NxTh/IQH78xSfXLZeKx7uMJ3T7THychdb3QcZg=";
      };
    });
  };

  pkgsUnstable = import inputs.nixpkgs-unstable {
    inherit (pkgs) system;
    overlays = [ claudeCodeOverlay rpiImagerOverlay ];
    config.allowUnfree = true;
  };

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

  # home-manager runs as a NixOS module (see flake.nix), so there is no
  # standalone `home-manager` CLI; `nixos-rebuild switch` applies this file.
  programs = {
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

  # Start/stop/restart user services on activation instead of only
  # printing suggestions, so newly added services run without a re-login.
  systemd.user.startServices = "sd-switch";

  systemd.user.services.stomach-reminder = {
    Unit.Description = "Hourly stomach exercise reminder";
    Service = {
      Type = "oneshot";
      ExecStart = "${pkgs.libnotify}/bin/notify-send -u critical 'Stomach exercises' 'Time to do your exercises'";
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

  # Expire this user's generations. NixOS's nix.gc only covers the system and
  # root profiles, so without this the per-user profiles under
  # ~/.local/state/nix/profiles (profile, home-manager, channels) accumulate
  # indefinitely and pin their whole closures -- that is what grew /nix/store
  # to 125GB against a 25GB live set.
  #
  # home-manager's own nix.gc module would express this, but it only exists in
  # 24.05+ and flake.nix pins home-manager to release-23.11.
  systemd.user.services.nix-gc = {
    Unit.Description = "Expire old Nix user generations and collect garbage";
    Service = {
      Type = "oneshot";
      # System nix, not pkgs.nix, to stay on the same version as the daemon.
      ExecStart = "/run/current-system/sw/bin/nix-collect-garbage --delete-older-than 7d";
    };
  };

  systemd.user.timers.nix-gc = {
    Unit.Description = "Weekly Nix user garbage collection";
    Timer = {
      OnCalendar = "weekly";
      Persistent = true;
      RandomizedDelaySec = 1800;
    };
    Install.WantedBy = [ "timers.target" ];
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
      pkgsUnstable.nodejs
      pkgsUnstable.yarn
      pkgsUnstable.youplot
      pkgsUnstable.rpi-imager
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
