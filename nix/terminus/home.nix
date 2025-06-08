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
  home.stateVersion = "25.05";
  programs.home-manager.enable = true;

  imports = [ ../files/dconf.nix ];

  home = {
    username = "craig";
    homeDirectory = "/home/craig";
    packages = [
      pkgsUnstable.emacs
      pkgsUnstable.git
      pkgsUnstable.firefox
      pkgsUnstable.ghostty
      pkgsUnstable.shutter
      pkgsUnstable.zig
      pkgsUnstable.go
      pkgs.xclip
      pkgs.dconf
      pkgs.keychain
    ];

    file = {
      ".config/i3status/config".source = ../files/i3status;
      ".config/gtk-3.0/settings.ini".source = ../files/gtk.ini;
      ".config/ghostty/config".source = ./files/ghostty.conf;
      ".zshrc".source = ../files/zshrc;
      ".emacs.d" = {
        source = ../files/emacs;
        recursive = true;
      };
    };
  };

  programs = {
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
  };

  nixpkgs = {
    config.allowUnfree = true;
  };  
}
