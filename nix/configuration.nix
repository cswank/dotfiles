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

  networking.hostName = "ba"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
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
    # unstable.alsaLib
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
    unstable.python2
    unstable.openssl
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.bash.enableCompletion = true;
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # List services that you want to enable:

  # Aka: bonjour
  services.avahi = { enable = true; nssmdns = true; };

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [ 22 80 443 3333 3334 6111 8080 50051 ];
  networking.firewall.allowedUDPPorts = [ 5353 ];
  networking.firewall.allowedUDPPortRanges = [
    { from = 60000; to = 61000; }
  ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  services.printing.enable = true;

  services.cron = {
    enable = true;
    systemCronJobs = [
      "0 22 * * * craig rsync -r /mnt/music/flac /mnt/backup/craig/Music/flac"
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

  systemd.services.quimby = {
   description = "quimby";

   serviceConfig = {
     User = "craig";
     EnvironmentFile=/home/craig/.quimby/env;
     WorkingDirectory=/home/craig/.quimby;
     ExecStart = "/usr/local/bin/quimby serve";
     Restart = "on-failure";
   };

   wantedBy = [ "default.target" ];
  };

  systemd.services.quimby.enable = true;

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
    openssh.authorizedKeys.keys = [ "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC50R5Zm5+lVl1vObuAbliMP+dZBLgZT8JQo6okGxy14PRrCbWkkJkQJCdXc5sc9U2/Qbmf3R70ytll/TyAx5JxTQ7lO5XCDEyOCvO7hxNJZbDwAWKJg1rljChtpei0cUZauLDuqVKxxDFOozRxYOwEZXw3cQ2KhjH/SvWHhIO32nSdhwbUaAdNbOUiHa4xrj16Ur/KEwXgD3B7FV6zCCv1Di6y0y/Ho9TMRIWU1Jx7v/pXy4x/o+a4S8H8iEZgSbPVsPV+1UrgfQrryBq1rCm7MuW03FmbcKGKVkcae+D9ocGu9cpbXFHPPBxpKqjwMPZtvFzn8K47I++89RDW8ch1 craig@Craigs-iMac.local" ];
  };
 

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.03"; # Did you read the comment?

}
