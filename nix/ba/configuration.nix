# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

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

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  # boot.loader.grub.efiSupport = true;
  # boot.loader.grub.efiInstallAsRemovable = true;
  # boot.loader.efi.efiSysMountPoint = "/boot/efi";
  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sda"; # or "nodev" for efi only

  boot.kernelModules = [ "usb-serial" "cp210x" ];

  networking.hostName = "ba"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Select internationalisation properties.
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  fonts.fonts = with pkgs; [
  dina-font
];

  i18n = { 
    defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  # time.timeZone = "Europe/Amsterdam";

  environment.systemPackages = with pkgs; [
    unstable.emacs 
    unstable.zsh 
    unstable.mosh 
    unstable.git
    unstable.go
    unstable.dep
    unstable.alsaLib
    unstable.gcc
    unstable.keychain
    unstable.screen
    unstable.htop
    unstable.usbutils
    unstable.hfsprogs
    unstable.abcde
    unstable.cdparanoia
    unstable.flac
    unstable.zip
    unstable.unzip
    unstable.wget
    unstable.cron
    unstable.pv
    unstable.avahi
    unstable.bashburn
    unstable.parted
    unstable.gnumake
    unstable.pkg-config
    unstable.cifs-utils
    unstable.traceroute
    unstable.bind
    unstable.cacert
    unstable.platformio
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.bash.enableCompletion = true;
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # List services that you want to enable:

  # Aka: bonjour
  services.avahi = { 
    enable = true; 
    nssmdns = true; 
    ipv4 = true;
    ipv6 = true;
    publish = {
      enable = true;
      addresses = true;
      workstation = true;
      userServices = true;
    };
  };

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.openssh.passwordAuthentication = false;

  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [ 22 80 443 631 2049 3333 3334 5355 6111 8000 8080 50051 50100 ];
  networking.firewall.allowedUDPPorts = [ 631 5353 ];
  networking.firewall.allowedUDPPortRanges = [
    { from = 60000; to = 61000; }
  ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS
  services.printing.enable = true;
  services.printing.browsing = true;
  services.printing.drivers = [ pkgs.brlaser ];
  services.printing.listenAddresses = [ "*:631" ];
  services.printing.defaultShared = true;
  services.printing.allowFrom = [ "all" ];
  services.printing.extraConf = ''
ServerName ba.local
  '';

  services.cron = {
    enable = true;
    systemCronJobs = [
      "0 22 * * *	craig	rsync -r /mnt/music/flac /mnt/backup/craig/Music/flac"
      "0 * * * *	craig	/home/craig/bin/dyn.sh"
      "0 1 * * *	root	systemctl restart quimby"
    ];
  };

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  hardware.pulseaudio.configFile = pkgs.runCommand "default.pa" {} ''
    sed 's/module-udev-detect$/module-udev-detect tsched=0/' \
      ${pkgs.pulseaudio}/etc/pulse/default.pa > $out
  '';
  
  sound.extraConfig =
    ''
      defaults.pcm.card 1
      defaults.ctl.card 1
      defaults.pcm.rate_converter "speexrate_best"
    '';

  systemd.services.mcli = {
   description = "mcli";

   serviceConfig = {
     User = "craig";
     Environment = ["MCLI_HOME=/home/craig/.mcli" "MCLI_MUSIC_LOCATION=/mnt/music" "MCLI_HOST=ba.local:50051"];
     ExecStart = "/home/craig/go/bin/mcli --serve";
     Restart = "on-failure";
   };

   wantedBy = [ "default.target" ];
  };

  systemd.services.mcli.enable = true;

  systemd.services.quimby = {
   description = "quimby";

   serviceConfig = {
     User = "quimby";
     EnvironmentFile=/home/quimby/.quimby/env;
     WorkingDirectory=/home/quimby/.quimby;
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
     EnvironmentFile=/home/proxy/.proxy/env;
     WorkingDirectory=/home/proxy/www;
     ExecStart = "/usr/local/bin/proxy";
     Restart = "on-failure";
   };

   wantedBy = [ "default.target" ];
  };

  systemd.services.proxy.enable = true;

  # Enable the X11 windowing system.
  # services.xserver.enable = true;
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable touchpad support.
  # services.xserver.libinput.enable = true;

  # Enable the KDE Desktop Environment.
  # services.xserver.displayManager.sddm.enable = true;
  # services.xserver.desktopManager.plasma5.enable = true;

  services.wakeonlan.interfaces = [ { interface = "enp1s0"; method = "magicpacket"; }] ;
  powerManagement.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  # users.extraUsers.guest = {
  #   isNormalUser = true;
  #   uid = 1000;
  # };

  users.users.craig =
  { isNormalUser = true;
    home = "/home/craig";
    description = "Craig";
    shell = pkgs.zsh;
    extraGroups = [ "wheel" "networkmanager" "audio" "cdrom" "cron" "dialout" ];
    openssh.authorizedKeys.keys = [ 
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC50R5Zm5+lVl1vObuAbliMP+dZBLgZT8JQo6okGxy14PRrCbWkkJkQJCdXc5sc9U2/Qbmf3R70ytll/TyAx5JxTQ7lO5XCDEyOCvO7hxNJZbDwAWKJg1rljChtpei0cUZauLDuqVKxxDFOozRxYOwEZXw3cQ2KhjH/SvWHhIO32nSdhwbUaAdNbOUiHa4xrj16Ur/KEwXgD3B7FV6zCCv1Di6y0y/Ho9TMRIWU1Jx7v/pXy4x/o+a4S8H8iEZgSbPVsPV+1UrgfQrryBq1rCm7MuW03FmbcKGKVkcae+D9ocGu9cpbXFHPPBxpKqjwMPZtvFzn8K47I++89RDW8ch1 craig@Craigs-iMac.local" "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDV2gfMm9saRThs+/Z0+hK8gvyCOKB0rxwv1MUSy+IvwJ/iYXBo6ws7+091CurdfR6VbAxLYFTGPyt3aBehd48F9HGJ7LN6EARLFkw+62XEgee2YVlZ2LicBwygoLL/OLseHHW+Wx+3JPv4ZTt4ThGClHDl4PE7cMogz0/ZvjmNpbUhyNbm/4bbsXcN/dkQaSz9Bbtfb7MV5yDwvfMkV7dC4jMZoDUPMXlZ+IzSdYAqMsqdqbaHzzqGhWgbY4PWJ2pCG64WXUuaeMJSdQtyGby/Mm8gXKAoPPD94Q5vGN9BLrFkTIlEEKcjkjTy8XawWoyaCeif+ZMVe9HPrmyThDPX Generated By Termius"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCy/7Lrqq1UqnD95ZtKqCcH0P+iJJZIZhDaQFXWasBkTPAn4w6SeiZvl+n30IkkG2Lr4rafW91VwEoAkooqDKJTadpKe+v23C0NJFreEuWVsFcAP2FqpRhl4cp4qn+80GP6kbFDgUDofSzGHJZwmqITzDK7tu4QwR6F4TtgDOI3gzH+ulJqDa7omrXQOOkMJ2mmDbJWhS5RkYeXPiqhZH50GqT1R3ZxxjwY8R2FB4HbzchuX5mkZpw6+wXfFnVSKsF6yXAB9ITT1YXJIooPqYtwRZa9cylfyU5ZLuMgPST/bg+vP4nqA65iNOeftPEvNsTBvSAswh6mpgMsr1k4CzGP craig@Sabeto.local"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC0wI0DmBLKI4uNo1a6KrNkU1I9CXZqWpsH4wBmk3AxC1mOP1iYOHcED47Fl0ZSJqR9Ea5K3EUQpTXhRQlSdThdhRnKvO+wjdtqux69ijIf53PhRNmqy5yMTWWTCxJ5aOxTR2HN/Pxe294za1W2nNdSRZmVtqlDwSvrEc4cwxJrLYfrcQS0MEwVkjnIJaFCpbihpSB5/ROvKLVKWw4iNlA3ed7//s366Wopck6aTlrhkcMaq1V0TKyP26q8EWT34h3E6i8V9Q2rV9ehlS26UK0ApW1nphF1pccnagYUBIK6IWs04PH2BgAhe8315Wj9EkjhW/JWEZxyj1+w9qKwGWbP craigswank@Craigs-MacBook-Pro.local"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCtGCJvFD4OO5d/u/kVm5pWaCSZ6s4ti3IktnK4KDAgIUH9Dh0tAk0kh5g1SYi3yiQ33CE3OpAxKbJ7U0+f4qyqT5B5D3AZ2LtX6YqitT0S0loYdipJ0/eggkUADvlIYU9M0RYra7Pb5xqXjRmxiFQTVT8Tphkt3nlRIysoERoKSJE7TYD2Wi4XmM3PzP2fO4ulV+xaVwmRydn7GXtqHE9KVDZXwUU89B5CLbpK0+u2AeZ9K2PSKA1NLMIJ/LOv7/MjabV3ZSCNkfaG2zw9RarSh48qpqNT3+V2VDDk5CoojIaUkYwBX7gZcYEWdcBicfzzBvLc1kml1A7QvWLE/O/F craigswank@Craigs-MacBook-Pro.local"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDZqIeMUlW9zTxADY6M4VShlFn4a65hOpFlEaOupLt3GXzL2cIrBLnfVqo2mV6M3paerg9XsXifkS8xLnjv9Urs6+v2peePJghY8eyLrZS5UgV8fsx7el5DSU1SfSUi8NFnloHD2WkrVvJj9DBaLbWWiEtFQQucjT9uRJoxk6nOnCOe4dLmgWWdgPUAdu/1UAABtI5V2MU3cjI3D9jl+dWammF7TF/CKH6cK8p9txO/+nFyf0Y9ZWX60XpAQ+gPDVhbuB1IlD6g+NozMRNBiA23veF4k76srsSLgdpywqzJQCYGvn8flKx2pQW/MeRnjGFoUg/jMR3bCy6+OiG3zgZ5V+Io57Fma6VE0AGwWiHA+lYhyc8JWaNCoDllfeyyXAfAhCTLd8+SoXBjpNdI0fOukwPzKfZNc6/qaeAZ6J25HgOihpT+mPHW5DBrz1jKL9jlkUqe8ifICdtLMmYt2Rsu3KFAi+JCyEAkvmi0MpY2Otpjj9tSYS5UfrY9TDd3/E8= craigswank@Craigs-Mac-mini.local"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQD3s72cP0bOpqFnUW6ByP4VuVmbzj4zYzohkA0qrbFla+V38ViRKrhugE1/NyfUz5KFsGZAXyZB13FS1G81IGgzvapXeyp2Rem0GgVqrcVY+F/DqTqLWv5TFNWswDa2pG3JO9+Dtr9hIjLFDn1rbmK4PnDIq4e5t2bTttaj4VZbmfWlRIO/cLnRZ6m+qAvG7O3a0BVNM/fmGlYzwJ7BXuHmyxCEE34stxSdVnS8dynBZCI2obe+jxynEM9kcRSNR3YnnQy/fbwIlNqe+Ci0CHSq5htVKNKP0z1QO6i8lA1YjeIvLMI0pssSEKyumZRWrtzb9/LqutOuqWTyQJZmRf2vXRqY6T5G+/Wq6fib/pcHcOqB2oa0QleM3OuB82MfAvlarLPQgv25VTys8utftCpRhCjVeG2aT86l1+yH2bUTkf+Kfmafu1VE5Naq1FJ0quVUfYSyBzCjvqL4JqXU/t8KALjsCDBoj9p+3kT15uK9UYZ6cy6ujcoQdnaCM5NFuQc= craig@nixos"];
  };

  users.users.quimby =
  { isNormalUser = true;
    home = "/home/quimby";
    description = "Quimby";
    shell = pkgs.zsh;
    extraGroups = [];
  };

  users.users.proxy =
  { isNormalUser = true;
    home = "/home/proxy";
    description = "Proxy";
    shell = pkgs.zsh;
    extraGroups = [];
  };

  fileSystems."/mnt/music" = {
    device = "/dev/disk/by-uuid/c0244566-01cd-4b1a-b2d0-7afd011f47a7";
    fsType = "ext4";
 };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "21.05"; # Did you read the comment?
  
}