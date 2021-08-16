{ config, pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "craig";
  home.homeDirectory = "/home/craig";

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


  home.packages = [
    pkgs.alacritty
    pkgs.konsole
    pkgs.guake
    pkgs.silver-searcher
  ];
  
}
