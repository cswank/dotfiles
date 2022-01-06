# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  environment.pathsToLink = [ "/libexec" ]; # links /libexec from derivations to /run/current-system/sw 

  nixpkgs.config.allowUnfree = true;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "terminus";
  time.timeZone = "America/Denver";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp0s10.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  services = {
    xserver = {
      enable = true;
      layout = "us";
      autoRepeatDelay = 200;
      autoRepeatInterval = 25;
      xkbOptions = "ctrl:swapcaps";
      desktopManager.xterm.enable = false;
      displayManager = {
        defaultSession = "none+i3";
      };
      
      windowManager.i3 = {
        enable = true;
        package = pkgs.i3-gaps;
        configFile = "/etc/i3.conf";
        extraPackages = with pkgs; [
          dmenu #application launcher most people use
          i3status # gives you the default i3 status bar
          i3lock #default i3 screen locker
        ];
      };
    };
    
    avahi = {
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
  };

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.craig = {
    isNormalUser = true;
    shell = pkgs.zsh;
    extraGroups = [ "wheel" "networkmanager" "audio" "dialout" ];
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC50R5Zm5+lVl1vObuAbliMP+dZBLgZT8JQo6okGxy14PRrCbWkkJkQJCdXc5sc9U2/Qbmf3R70ytll/TyAx5JxTQ7lO5XCDEyOCvO7hxNJZbDwAWKJg1rljChtpei0cUZauLDuqV
KxxDFOozRxYOwEZXw3cQ2KhjH/SvWHhIO32nSdhwbUaAdNbOUiHa4xrj16Ur/KEwXgD3B7FV6zCCv1Di6y0y/Ho9TMRIWU1Jx7v/pXy4x/o+a4S8H8iEZgSbPVsPV+1UrgfQrryBq1rCm7MuW03FmbcKGKVkcae+D9ocGu9cpbXFHPPBxpKqj
wMPZtvFzn8K47I++89RDW8ch1 craig@Craigs-iMac.local" "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDV2gfMm9saRThs+/Z0+hK8gvyCOKB0rxwv1MUSy+IvwJ/iYXBo6ws7+091CurdfR6VbAxLYFTGPyt3aBehd48F9HGJ7
LN6EARLFkw+62XEgee2YVlZ2LicBwygoLL/OLseHHW+Wx+3JPv4ZTt4ThGClHDl4PE7cMogz0/ZvjmNpbUhyNbm/4bbsXcN/dkQaSz9Bbtfb7MV5yDwvfMkV7dC4jMZoDUPMXlZ+IzSdYAqMsqdqbaHzzqGhWgbY4PWJ2pCG64WXUuaeMJSdQ
tyGby/Mm8gXKAoPPD94Q5vGN9BLrFkTIlEEKcjkjTy8XawWoyaCeif+ZMVe9HPrmyThDPX Generated By Termius"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCy/7Lrqq1UqnD95ZtKqCcH0P+iJJZIZhDaQFXWasBkTPAn4w6SeiZvl+n30IkkG2Lr4rafW91VwEoAkooqDKJTadpKe+v23C0NJFreEuWVsFcAP2FqpRhl4cp4qn+80GP6kbFDgU
DofSzGHJZwmqITzDK7tu4QwR6F4TtgDOI3gzH+ulJqDa7omrXQOOkMJ2mmDbJWhS5RkYeXPiqhZH50GqT1R3ZxxjwY8R2FB4HbzchuX5mkZpw6+wXfFnVSKsF6yXAB9ITT1YXJIooPqYtwRZa9cylfyU5ZLuMgPST/bg+vP4nqA65iNOeftPE
vNsTBvSAswh6mpgMsr1k4CzGP craig@Sabeto.local"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC0wI0DmBLKI4uNo1a6KrNkU1I9CXZqWpsH4wBmk3AxC1mOP1iYOHcED47Fl0ZSJqR9Ea5K3EUQpTXhRQlSdThdhRnKvO+wjdtqux69ijIf53PhRNmqy5yMTWWTCxJ5aOxTR2HN/P
xe294za1W2nNdSRZmVtqlDwSvrEc4cwxJrLYfrcQS0MEwVkjnIJaFCpbihpSB5/ROvKLVKWw4iNlA3ed7//s366Wopck6aTlrhkcMaq1V0TKyP26q8EWT34h3E6i8V9Q2rV9ehlS26UK0ApW1nphF1pccnagYUBIK6IWs04PH2BgAhe8315Wj
9EkjhW/JWEZxyj1+w9qKwGWbP craigswank@Craigs-MacBook-Pro.local"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCtGCJvFD4OO5d/u/kVm5pWaCSZ6s4ti3IktnK4KDAgIUH9Dh0tAk0kh5g1SYi3yiQ33CE3OpAxKbJ7U0+f4qyqT5B5D3AZ2LtX6YqitT0S0loYdipJ0/eggkUADvlIYU9M0RYra7Pb5xqXjRmxiFQTVT8Tphkt3nlRIysoERoKSJE7TYD2Wi4XmM3PzP2fO4ulV+xaVwmRydn7GXtqHE9KVDZXwUU89B5CLbpK0+u2AeZ9K2PSKA1NLMIJ/LOv7/MjabV3ZSCNkfaG2zw9RarSh48qpqNT3+V2VDDk5CoojIaUkYwBX7gZcYEWdcBicfzzBvLc1kml1A7QvWLE/O/F craigswank@Craigs-MacBook-Pro.local"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDZqIeMUlW9zTxADY6M4VShlFn4a65hOpFlEaOupLt3GXzL2cIrBLnfVqo2mV6M3paerg9XsXifkS8xLnjv9Urs6+v2peePJghY8eyLrZS5UgV8fsx7el5DSU1SfSUi8NFnloHD2WkrVvJj9DBaLbWWiEtFQQucjT9uRJoxk6nOnCOe4dLmgWWdgPUAdu/1UAABtI5V2MU3cjI3D9jl+dWammF7TF/CKH6cK8p9txO/+nFyf0Y9ZWX60XpAQ+gPDVhbuB1IlD6g+NozMRNBiA23veF4k76srsSLgdpywqzJQCYGvn8flKx2pQW/MeRnjGFoUg/jMR3bCy6+OiG3zgZ5V+Io57Fma6VE0AGwWiHA+lYhyc8JWaNCoDllfeyyXAfAhCTLd8+SoXBjpNdI0fOukwPzKfZNc6/qaeAZ6J25HgOihpT+mPHW5DBrz1jKL9jlkUqe8ifICdtLMmYt2Rsu3KFAi+JCyEAkvmi0MpY2Otpjj9tSYS5UfrY9TDd3/E8= craigswank@Craigs-Mac-mini.local"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQD3s72cP0bOpqFnUW6ByP4VuVmbzj4zYzohkA0qrbFla+V38ViRKrhugE1/NyfUz5KFsGZAXyZB13FS1G81IGgzvapXeyp2Rem0GgVqrcVY+F/DqTqLWv5TFNWswDa2pG3JO9+Dtr9hIjLFDn1rbmK4PnDIq4e5t2bTttaj4VZbmfWlRIO/cLnRZ6m+qAvG7O3a0BVNM/fmGlYzwJ7BXuHmyxCEE34stxSdVnS8dynBZCI2obe+jxynEM9kcRSNR3YnnQy/fbwIlNqe+Ci0CHSq5htVKNKP0z1QO6i8lA1YjeIvLMI0pssSEKyumZRWrtzb9/LqutOuqWTyQJZmRf2vXRqY6T5G+/Wq6fib/pcHcOqB2oa0QleM3OuB82MfAvlarLPQgv25VTys8utftCpRhCjVeG2aT86l1+yH2bUTkf+Kfmafu1VE5Naq1FJ0quVUfYSyBzCjvqL4JqXU/t8KALjsCDBoj9p+3kT15uK9UYZ6cy6ujcoQdnaCM5NFuQc= craig@nixos"];
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    emacs
    avahi
    zsh
    git
    paprefs
    pasystray
    firefox
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };  

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?
}
