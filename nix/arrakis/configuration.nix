# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).

{ config, lib, pkgs, ... }:

let
 unstableTarball =
   fetchTarball
     https://github.com/NixOS/nixpkgs-channels/archive/nixpkgs-unstable.tar.gz;
in
{
  imports =
    [
      ./hardware-configuration.nix
    ];

  nixpkgs.config = {
    packageOverrides = pkgs: {
      unstable = import unstableTarball {
        config = config.nixpkgs.config;
      };
    };
  };

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "arrakis";
  time.timeZone = "America/Denver";

  services.xserver.xkb.layout = "us";

  environment.systemPackages = with pkgs; [
    pkgs.emacs
    pkgs.mosh 
    pkgs.git
    pkgs.keychain
    pkgs.screen
    pkgs.bind
    pkgs.wakelan
    unstable.htop
    unstable.acme-sh
  ];

  programs.zsh.enable = true;

  users.users.craig = {
     isNormalUser = true;
     shell = pkgs.zsh;
     extraGroups = [ "wheel" "cron" ];
     openssh.authorizedKeys.keys = [
       "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIANA5nbUglGbrSDJuXg+POTOQahpt6fqnbQyEditR3Zb craigswank@Craigs-MacBook-Pro-2.local"
       "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKl0QUrClfkHvvN4HQIHH7KjgshLVCBQZnfoJKtGov/i craig@trantor"
       "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAtQEzzTxy/ay4aZlZDnjXcj6xSElp4WhhL9Zc6a8pau craig@terminus"
       "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIP8bzpDmgoIgDQCI6UFPW0DG7jMnwuL6bBxMAyjPCWMR craig@Craigs-MBP.localdomain"
     ];
  };

  users.users.quimby = {
    isNormalUser = true;
    home = "/home/quimby";
    description = "Quimby";
    shell = pkgs.zsh;
    extraGroups = [];
  };

  users.users.proxy = {
    isNormalUser = true;
    home = "/home/proxy";
    description = "Proxy";
    shell = pkgs.zsh;
    extraGroups = [];
  };

  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "no";
      PasswordAuthentication = false;
    };
  };

  networking.firewall = {
    allowedTCPPorts = [ 22 80 443 631 2049 3333 3334 5355 6111 6114 8000 8080 50051 50100 ];
    allowedUDPPorts = [ 631 5353 ];
    allowedUDPPortRanges = [
      { from = 60000; to = 61000; }
    ];
  };

  services.avahi = { 
    enable = true; 
    nssmdns4 = true; 
    ipv4 = true;
    ipv6 = true;
    publish = {
      enable = true;
      addresses = true;
      workstation = true;
      userServices = true;
    };
  };

  services.cron = {
    enable = true;
    systemCronJobs = [
      "0 2 15 Aug,Nov,Feb,May *	root	/root/bin/certs.sh >> /var/log/certs.log 2>&1"
      "0 * * * *	craig	/home/craig/bin/dyn.sh"
      "0 1 * * *	root	systemctl restart quimby"
    ];
  };

  systemd.services.quimby = {
    description = "quimby";

    serviceConfig = {
      User = "quimby";
      EnvironmentFile=/home/quimby/env;
      WorkingDirectory=/home/quimby;
      ExecStart = "/usr/local/bin/quimby serve";
      Restart = "on-failure";
    };

    wantedBy = [ "default.target" ];
  };  

  systemd.services.quimby.enable = true;

  systemd.services.proxy = {
    description = "proxy";

    serviceConfig = {
      User = "proxy";
      EnvironmentFile=/home/proxy/env;
      WorkingDirectory=/mnt/documents/www;
      ExecStart = "/usr/local/bin/proxy";
      Restart = "on-failure";
      AmbientCapabilities="CAP_NET_BIND_SERVICE";
    };

    wantedBy = [ "default.target" ];
  };

  systemd.services.proxy.enable = true;

  fileSystems."/mnt/music" = {
    device = "/dev/disk/by-uuid/34ebd659-f4b2-4b38-95d8-6e7922749a33";
    fsType = "ext4";
 };

  fileSystems."/mnt/documents" = {
    device = "/dev/disk/by-uuid/4c134747-97c6-4498-a2c8-b0619e46c742";
    fsType = "ext4";
  };

  # Do NOT change this value unless you have manually inspected all the changes it would make to your configuration,
  # and migrated your data accordingly.
  # For more information, see `man configuration.nix` or https://nixos.org/manual/nixos/stable/options#opt-system.stateVersion .
  system.stateVersion = "25.05"; # Did you read the comment?
}
