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
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  nixpkgs.config = {
    packageOverrides = pkgs: {
      unstable = import unstableTarball {
        config = config.nixpkgs.config;
      };
    };
  };

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "arrakis";
  time.timeZone = "America/Denver";

  # Configure keymap in X11
  services.xserver.xkb.layout = "us";

  environment.systemPackages = with pkgs; [
    pkgs.emacs
    pkgs.mosh 
    pkgs.git
    pkgs.keychain
    pkgs.screen
    unstable.acme-sh
  ];

  programs.zsh.enable = true;

  users.users.craig = {
     isNormalUser = true;
     shell = pkgs.zsh;
     extraGroups = [ "wheel" "cron" ];
     openssh.authorizedKeys.keys = [
       "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQC6lfhGGP3P8JJJZOG90dPBQImsYRYkbW+Bnb6gvczZB1Y8bcfOeY7Xxw5zwZaNjRpFExVsvIxPUEjQE2+bXFIHqdv0Ai0Qrtl6inYI82jnHf3j6rXF/GjYenVqkabtFMlCpyabyTKKVC4/fX1qcewsVR/FI/c3oUmze/CK4V8Rlrb/tsjptQYHVIr5NUOHyVObOQDXgsh+P9Bo5fqtRflt2lSFAbEDbqGEXixovZbEpBVbvbWbtc5o9pQ35QS4qh+1aSlBMbnlhMXjNFO0X+3HO44QtZRp3oLINTx6A15o232hfBjDCKw5NGF82PdP1VmtOTAkSOZKeUlD7fxNI6zwHpHLS7ybBr1sC9G4SbxqRCHb74COBT9KEkYRG5poI0BUhk3d/ZBSIm53EkSP11+gfq3N0MxGems9xry0HQBo/urvHs6yb8ImDJaOL9LW9kC2YfvwnQhCyxfnNjxDpZ2NfV37nh2BjSFiGC0wVgVqEOqClNrmDSRdaNHerh7Ke0U= craig@Craigs-MacBook-Pro.local"
       "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC0wI0DmBLKI4uNo1a6KrNkU1I9CXZqWpsH4wBmk3AxC1mOP1iYOHcED47Fl0ZSJqR9Ea5K3EUQpTXhRQlSdThdhRnKvO+wjdtqux69ijIf53PhRNmqy5yMTWWTCxJ5aOxTR2HN/Pxe294za1W2nNdSRZmVtqlDwSvrEc4cwxJrLYfrcQS0MEwVkjnIJaFCpbihpSB5/ROvKLVKWw4iNlA3ed7//s366Wopck6aTlrhkcMaq1V0TKyP26q8EWT34h3E6i8V9Q2rV9ehlS26UK0ApW1nphF1pccnagYUBIK6IWs04PH2BgAhe8315Wj9EkjhW/JWEZxyj1+w9qKwGWbP craigswank@Craigs-MacBook-Pro.local"
       "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCtGCJvFD4OO5d/u/kVm5pWaCSZ6s4ti3IktnK4KDAgIUH9Dh0tAk0kh5g1SYi3yiQ33CE3OpAxKbJ7U0+f4qyqT5B5D3AZ2LtX6YqitT0S0loYdipJ0/eggkUADvlIYU9M0RYra7Pb5xqXjRmxiFQTVT8Tphkt3nlRIysoERoKSJE7TYD2Wi4XmM3PzP2fO4ulV+xaVwmRydn7GXtqHE9KVDZXwUU89B5CLbpK0+u2AeZ9K2PSKA1NLMIJ/LOv7/MjabV3ZSCNkfaG2zw9RarSh48qpqNT3+V2VDDk5CoojIaUkYwBX7gZcYEWdcBicfzzBvLc1kml1A7QvWLE/O/F craigswank@Craigs-MacBook-Pro.local"
       "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDZqIeMUlW9zTxADY6M4VShlFn4a65hOpFlEaOupLt3GXzL2cIrBLnfVqo2mV6M3paerg9XsXifkS8xLnjv9Urs6+v2peePJghY8eyLrZS5UgV8fsx7el5DSU1SfSUi8NFnloHD2WkrVvJj9DBaLbWWiEtFQQucjT9uRJoxk6nOnCOe4dLmgWWdgPUAdu/1UAABtI5V2MU3cjI3D9jl+dWammF7TF/CKH6cK8p9txO/+nFyf0Y9ZWX60XpAQ+gPDVhbuB1IlD6g+NozMRNBiA23veF4k76srsSLgdpywqzJQCYGvn8flKx2pQW/MeRnjGFoUg/jMR3bCy6+OiG3zgZ5V+Io57Fma6VE0AGwWiHA+lYhyc8JWaNCoDllfeyyXAfAhCTLd8+SoXBjpNdI0fOukwPzKfZNc6/qaeAZ6J25HgOihpT+mPHW5DBrz1jKL9jlkUqe8ifICdtLMmYt2Rsu3KFAi+JCyEAkvmi0MpY2Otpjj9tSYS5UfrY9TDd3/E8= craigswank@Craigs-Mac-mini.local"
       "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQC6lfhGGP3P8JJJZOG90dPBQImsYRYkbW+Bnb6gvczZB1Y8bcfOeY7Xxw5zwZaNjRpFExVsvIxPUEjQE2+bXFIHqdv0Ai0Qrtl6inYI82jnHf3j6rXF/GjYenVqkabtFMlCpyabyTKKVC4/fX1qcewsVR/FI/c3oUmze/CK4V8Rlrb/tsjptQYHVIr5NUOHyVObOQDXgsh+P9Bo5fqtRflt2lSFAbEDbqGEXixovZbEpBVbvbWbtc5o9pQ35QS4qh+1aSlBMbnlhMXjNFO0X+3HO44QtZRp3oLINTx6A15o232hfBjDCKw5NGF82PdP1VmtOTAkSOZKeUlD7fxNI6zwHpHLS7ybBr1sC9G4SbxqRCHb74COBT9KEkYRG5poI0BUhk3d/ZBSIm53EkSP11+gfq3N0MxGems9xry0HQBo/urvHs6yb8ImDJaOL9LW9kC2YfvwnQhCyxfnNjxDpZ2NfV37nh2BjSFiGC0wVgVqEOqClNrmDSRdaNHerh7Ke0U= craig@Craigs-MacBook-Pro.local"
       "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQCyIr6FLKv+GeZ0MiGXWR76CoER9UICf3p6wOoeAPeAF9W/kRkPTLG8OS7cUIhVLVH6fz6PZSi45IMqVReHA1RosJa7keNH7pZWtRotAXHT/TkiYfiugfEYSVzZfnyn573q+6rUK3V+8KjDLFqRTGwSAMB6sfIiquYeAAJPw5/22YqlzYOL+TZXNNYtMfEL9sN6qF3kHRtCS6NnIUL+V5TIkrWuBJ7SWyV23PMTXKKT69hhYfnWfLH2NuZ2xQnH8joq4/65ZId4DYNnSGmBND9Fl9ayFuzKFCjz96rGzBKXy8nT8VZg+v5x7megV/OyOygwFO+cJadVHQ/Q1ErhbB35LZe9r9RfLptL1jXg7bn3xv0/k8GIrI/vTacweoQo97ftkf3o+2jketERdScypQaa23q5MtSiEXWjMPI+DUyOnWjaofyf52FWvfi8EAlnCB9Z/TFDQ6Wy/5/uD80WaHICe+eLImn/VK9UnXsFWxfVNH1mA4mPyrfKXt9YMT4FKtc= craig@aurora.local"
       "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKl0QUrClfkHvvN4HQIHH7KjgshLVCBQZnfoJKtGov/i craig@trantor"
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
