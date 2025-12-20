# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).

{ config, lib, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./i3.nix
    ];

  boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
      grub.device = "nodev";
    };
    kernelPackages = pkgs.linuxPackagesFor (pkgs.linux_6_6.override {
      argsOverride = rec {
        src = pkgs.fetchurl {
          url = "mirror://kernel/linux/kernel/v6.x/linux-${version}.tar.xz";
          sha256 = "8ebc65af0cfc891ba63dce0546583da728434db0f5f6a54d979f25ec47f548b3";
        };
        version = "6.6.9";
        modDirVersion = "6.6.9";
      };
    });
    resumeDevice = "/dev/nvme0n1p2";
  };

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.packageOverrides = pkgs: {
    vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
  };

  systemd.services.NetworkManager-wait-online.enable = false;

  systemd.services.usbwake = {
    enable = true;
    description = "enable wake from usb devices";
    unitConfig = {
      Type = "simple";
    };
    script = ''
    echo enabled > /sys/bus/usb/devices/usb1/power/wakeup
    echo enabled > /sys/bus/usb/devices/usb2/power/wakeup
    '';
    wantedBy = [ "multi-user.target" ];
  };

  time.timeZone = "America/Denver";

  networking = {
    hostName = "trantor";
    useDHCP = false;
    useNetworkd = true;

    interfaces = {
      enp2s0 = {
        useDHCP = false;
        wakeOnLan = {
          enable = true;
          policy = ["magic"];
        };
      };
      wlp3s0.useDHCP = false;
    };

    networkmanager.enable = true;
    #networkmanager.options = with pkgs; [ gnome3.networkmanager-openvpn ];

    firewall = {
      # 5353 for Avahi
      # 1883 for mqtt
      allowedTCPPorts = [ 7 9 22 1883 5353 8081 8883];
      allowedUDPPorts = [ 5353 7001 8883 ];
      allowedUDPPortRanges = [
        # mosh
        { from = 60000; to = 61000; }
      ];
    };
  };

  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  services = {
    xserver = {
      enable = true;
      layout = "us";
      dpi = 192;
      autoRepeatDelay = 200;
      autoRepeatInterval = 25;
      xkbOptions = "ctrl:swapcaps";
      desktopManager.xterm.enable = false;
      displayManager = {
        defaultSession = "none+i3";
        sessionCommands = ''
        xsetroot -solid '#07422f'
        '';
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

    # CUPS
    printing = {
      enable = true;
      browsing = true;
      drivers = [ pkgs.brlaser ];
    };

    postgresql = {
      enable = true;
      package = pkgs.postgresql_16.withPackages (p: [ p.postgis ]);
      dataDir = "/mnt/postgresql/16";
      enableTCPIP = true;
      authentication = pkgs.lib.mkOverride 16 ''
            local all       postgres trust
            local all       craig    trust
            host  all       all      127.0.0.1/32   scram-sha-256
            host  all       all      ::1/128        scram-sha-256
      '';
    };

    openssh = {
      enable = true;
      settings = {
        PasswordAuthentication = false;
      };
    };
  };

  sound.enable = true;

  hardware = {
    pulseaudio = {
      enable = true;
      configFile = pkgs.runCommand "default.pa" {} ''
    sed 's/module-udev-detect$/module-udev-detect tsched=0/' \
    ${pkgs.pulseaudio}/etc/pulse/default.pa > $out
    '';
    };

    bluetooth.enable = true;
    opengl = {
      enable = true;
      extraPackages = with pkgs; [
        intel-media-driver # LIBVA_DRIVER_NAME=iHD
        vaapiVdpau
        libvdpau-va-gl # LIBVA_DRIVER_NAME=i965
        vaapiIntel
      ];
    };
  };

  virtualisation.docker.enable = true;

  environment = {
    pathsToLink = [ "/libexec" ]; # links /libexec from derivations to /run/current-system/sw
    variables = {
	    GDK_SCALE = "2"; # Scale UI elements
	    GDK_DPI_SCALE = "0.5"; # Reverse scale the fonts
      MOZ_X11_EGL = "1";
      LIBVA_DRIVER_NAME = "iHD";
      VDPAU_DRIVER = "va_gl";
      WEBKIT_FORCE_SANDBOX = "0"; #nyxt crashes without this
    };
    
    systemPackages = with pkgs; [
      git
      avahi
      nssmdns
      mosh
      cifs-utils
      paprefs
      pasystray
      pavucontrol
      shairplay
    ];
  };

  users.users.craig = {
    isNormalUser = true;
    shell = pkgs.zsh;
    extraGroups = [ "wheel" "audio" "docker" ]; # Enable ‘sudo’ for the user.
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDZqIeMUlW9zTxADY6M4VShlFn4a65hOpFlEaOupLt3GXzL2cIrBLnfVqo2mV6M3paerg9XsXifkS8xLnjv9Urs6+v2peePJghY8eyLrZS5UgV8fsx7el5DSU1SfSUi8NFnloHD2WkrVvJj9DBaLbWWiEtFQQucjT9uRJoxk6nOnCOe4dLmgWWdgPUAdu/1UAABtI5V2MU3cjI3D9jl+dWammF7TF/CKH6cK8p9txO/+nFyf0Y9ZWX60XpAQ+gPDVhbuB1IlD6g+NozMRNBiA23veF4k76srsSLgdpywqzJQCYGvn8flKx2pQW/MeRnjGFoUg/jMR3bCy6+OiG3zgZ5V+Io57Fma6VE0AGwWiHA+lYhyc8JWaNCoDllfeyyXAfAhCTLd8+SoXBjpNdI0fOukwPzKfZNc6/qaeAZ6J25HgOihpT+mPHW5DBrz1jKL9jlkUqe8ifICdtLMmYt2Rsu3KFAi+JCyEAkvmi0MpY2Otpjj9tSYS5UfrY9TDd3/E8= craigswank@Craigs-Mac-mini.local"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC0wI0DmBLKI4uNo1a6KrNkU1I9CXZqWpsH4wBmk3AxC1mOP1iYOHcED47Fl0ZSJqR9Ea5K3EUQpTXhRQlSdThdhRnKvO+wjdtqux69ijIf53PhRNmqy5yMTWWTCxJ5aOxTR2HN/Pxe294za1W2nNdSRZmVtqlDwSvrEc4cwxJrLYfrcQS0MEwVkjnIJaFCpbihpSB5/ROvKLVKWw4iNlA3ed7//s366Wopck6aTlrhkcMaq1V0TKyP26q8EWT34h3E6i8V9Q2rV9ehlS26UK0ApW1nphF1pccnagYUBIK6IWs04PH2BgAhe8315Wj9EkjhW/JWEZxyj1+w9qKwGWbP craigswank@Craigs-MacBook-Pro.local"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCtGCJvFD4OO5d/u/kVm5pWaCSZ6s4ti3IktnK4KDAgIUH9Dh0tAk0kh5g1SYi3yiQ33CE3OpAxKbJ7U0+f4qyqT5B5D3AZ2LtX6YqitT0S0loYdipJ0/eggkUADvlIYU9M0RYra7Pb5xqXjRmxiFQTVT8Tphkt3nlRIysoERoKSJE7TYD2Wi4XmM3PzP2fO4ulV+xaVwmRydn7GXtqHE9KVDZXwUU89B5CLbpK0+u2AeZ9K2PSKA1NLMIJ/LOv7/MjabV3ZSCNkfaG2zw9RarSh48qpqNT3+V2VDDk5CoojIaUkYwBX7gZcYEWdcBicfzzBvLc1kml1A7QvWLE/O/F craigswank@Craigs-MacBook-Pro.local"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQC6lfhGGP3P8JJJZOG90dPBQImsYRYkbW+Bnb6gvczZB1Y8bcfOeY7Xxw5zwZaNjRpFExVsvIxPUEjQE2+bXFIHqdv0Ai0Qrtl6inYI82jnHf3j6rXF/GjYenVqkabtFMlCpyabyTKKVC4/fX1qcewsVR/FI/c3oUmze/CK4V8Rlrb/tsjptQYHVIr5NUOHyVObOQDXgsh+P9Bo5fqtRflt2lSFAbEDbqGEXixovZbEpBVbvbWbtc5o9pQ35QS4qh+1aSlBMbnlhMXjNFO0X+3HO44QtZRp3oLINTx6A15o232hfBjDCKw5NGF82PdP1VmtOTAkSOZKeUlD7fxNI6zwHpHLS7ybBr1sC9G4SbxqRCHb74COBT9KEkYRG5poI0BUhk3d/ZBSIm53EkSP11+gfq3N0MxGems9xry0HQBo/urvHs6yb8ImDJaOL9LW9kC2YfvwnQhCyxfnNjxDpZ2NfV37nh2BjSFiGC0wVgVqEOqClNrmDSRdaNHerh7Ke0U= craig@Craigs-MacBook-Pro.local"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIP8bzpDmgoIgDQCI6UFPW0DG7jMnwuL6bBxMAyjPCWMR craig@Craigs-MBP.localdomain"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIC/d4tqHOjIukqYWixQAfQrt1dLpaM8o9bYwyk7Ct8pi craig@arrakis"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAtQEzzTxy/ay4aZlZDnjXcj6xSElp4WhhL9Zc6a8pau craig@terminus"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIANA5nbUglGbrSDJuXg+POTOQahpt6fqnbQyEditR3Zb craigswank@Craigs-MacBook-Pro-2.local"      
    ];
  };

  fonts = {
    packages = with pkgs; [
      corefonts
      dejavu_fonts
      dina-font
      fira-code
      fira-code-symbols
      liberation_ttf
      material-design-icons
      noto-fonts
      noto-fonts-cjk
      noto-fonts-emoji
      powerline-fonts
      proggyfonts
      roboto
      siji
      source-code-pro
      source-sans-pro
      source-serif-pro
      terminus_font
      ubuntu_font_family
    ];

    fontconfig.defaultFonts = {
      monospace = [
        "DejaVu Sans Mono"
      ];
      sansSerif = [
        "DejaVu Sans"
      ];
      serif = [
        "DejaVu Serif"
      ];
    };
  };

  programs = {
    zsh.enable = true;
    dconf.enable = true;
    nm-applet.enable = true;

    # Some programs need SUID wrappers, can be configured further or are
    # started in user sessions.
    # programs.mtr.enable = true;
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
      pinentryFlavor = "curses";
    };
  };

  system.activationScripts = {
      mnt = {
        text = ''
          if [ ! -d /mnt/postgresql/13 ] ; then
            mkdir -p /mnt/postgresql/13
            chown -R postgres:postgres /mnt/postgresql/13
          fi
        '';
        deps = [];
      };
   };

  # rds-ca-2019-root.pem
  security.pki.certificates = [ "-----BEGIN CERTIFICATE-----
MIIEBjCCAu6gAwIBAgIJAMc0ZzaSUK51MA0GCSqGSIb3DQEBCwUAMIGPMQswCQYD
VQQGEwJVUzEQMA4GA1UEBwwHU2VhdHRsZTETMBEGA1UECAwKV2FzaGluZ3RvbjEi
MCAGA1UECgwZQW1hem9uIFdlYiBTZXJ2aWNlcywgSW5jLjETMBEGA1UECwwKQW1h
em9uIFJEUzEgMB4GA1UEAwwXQW1hem9uIFJEUyBSb290IDIwMTkgQ0EwHhcNMTkw
ODIyMTcwODUwWhcNMjQwODIyMTcwODUwWjCBjzELMAkGA1UEBhMCVVMxEDAOBgNV
BAcMB1NlYXR0bGUxEzARBgNVBAgMCldhc2hpbmd0b24xIjAgBgNVBAoMGUFtYXpv
biBXZWIgU2VydmljZXMsIEluYy4xEzARBgNVBAsMCkFtYXpvbiBSRFMxIDAeBgNV
BAMMF0FtYXpvbiBSRFMgUm9vdCAyMDE5IENBMIIBIjANBgkqhkiG9w0BAQEFAAOC
AQ8AMIIBCgKCAQEArXnF/E6/Qh+ku3hQTSKPMhQQlCpoWvnIthzX6MK3p5a0eXKZ
oWIjYcNNG6UwJjp4fUXl6glp53Jobn+tWNX88dNH2n8DVbppSwScVE2LpuL+94vY
0EYE/XxN7svKea8YvlrqkUBKyxLxTjh+U/KrGOaHxz9v0l6ZNlDbuaZw3qIWdD/I
6aNbGeRUVtpM6P+bWIoxVl/caQylQS6CEYUk+CpVyJSkopwJlzXT07tMoDL5WgX9
O08KVgDNz9qP/IGtAcRduRcNioH3E9v981QO1zt/Gpb2f8NqAjUUCUZzOnij6mx9
McZ+9cWX88CRzR0vQODWuZscgI08NvM69Fn2SQIDAQABo2MwYTAOBgNVHQ8BAf8E
BAMCAQYwDwYDVR0TAQH/BAUwAwEB/zAdBgNVHQ4EFgQUc19g2LzLA5j0Kxc0LjZa
pmD/vB8wHwYDVR0jBBgwFoAUc19g2LzLA5j0Kxc0LjZapmD/vB8wDQYJKoZIhvcN
AQELBQADggEBAHAG7WTmyjzPRIM85rVj+fWHsLIvqpw6DObIjMWokpliCeMINZFV
ynfgBKsf1ExwbvJNzYFXW6dihnguDG9VMPpi2up/ctQTN8tm9nDKOy08uNZoofMc
NUZxKCEkVKZv+IL4oHoeayt8egtv3ujJM6V14AstMQ6SwvwvA93EP/Ug2e4WAXHu
cbI1NAbUgVDqp+DRdfvZkgYKryjTWd/0+1fS8X1bBZVWzl7eirNVnHbSH2ZDpNuY
0SBd8dj5F6ld3t58ydZbrTHze7JJOd8ijySAp4/kiu9UfZWuTPABzDa/DSdz9Dk/
zPW4CXXvhLmE02TA9/HeCw3KEHIwicNuEfw=
-----END CERTIFICATE-----" ];

  # Do NOT change this value unless you have manually inspected all the changes it would make to your configuration,
  # and migrated your data accordingly.
  #
  # For more information, see `man configuration.nix` or https://nixos.org/manual/nixos/stable/options#opt-system.stateVersion .
  system.stateVersion = "23.11"; # Did you read the comment?

}
