# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./i3.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
    resumeDevice = "/dev/nvme0n1p2";
  };

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.packageOverrides = pkgs: {
    vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
  };

  time.timeZone = "America/Denver";

  networking = {
    hostName = "trantor";
    useDHCP = false;
    useNetworkd = true;

    interfaces = {
      enp2s0.useDHCP = false;
      wlp3s0.useDHCP = false;
    };

    networkmanager.enable = true;
    networkmanager.packages = with pkgs; [ gnome3.networkmanager-openvpn ];

    firewall = {
      # 5353 for Avahi
      allowedTCPPorts = [ 7 9 22 5353 8081 ];
      allowedUDPPorts = [ 5353 7001 ];
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
      package = pkgs.postgresql_13.withPackages (p: [ p.postgis ]);
      dataDir = "/mnt/postgresql/13";
    };

    openssh = {
      enable = true;
      passwordAuthentication = false;
    };

    wakeonlan.interfaces = [{ interface = "enp2s0"; method = "magicpacket"; }];
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
        libvdpau-va-gl

        # vaapiIntel
        # libvdpau
        # libvdpau-va-gl
        # vaapiVdpau
      ];
    };
  };

  environment = {
    pathsToLink = [ "/libexec" ]; # links /libexec from derivations to /run/current-system/sw
    variables = {
	    GDK_SCALE = "2"; # Scale UI elements
	    GDK_DPI_SCALE = "0.5"; # Reverse scale the fonts
      MOZ_X11_EGL = "1";
      LIBVA_DRIVER_NAME = "iHD";
      VDPAU_DRIVER = "va_gl";
    };
    systemPackages = with pkgs; [
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
    extraGroups = [ "wheel" "audio" ]; # Enable ‘sudo’ for the user.
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDQbVm/bXNum2iknQeV0vEcDc/SkHXImMTQvAKmtMDYHkgmRoBswExG34B4qc8MMdbGDfyOLngVHTcSC8KVv2VP90YY6l+uNKxsBHt5KX4I7OycaDPYUwFdMEAsenWHPn5GMtM5lXlAX8BcpZSmoU99fm7KJjgfkoI/wWAsxX5D8ZhYQmP1SnrsaiPTGNUtfEnkAFedBax9jStwyGPTV1WGc/EjchZL9Ryu7myZFE8R6bEicF+VlEuk1XlH+9wa0lH/znsqwr7jIOs7TxqfQVVLFWrwE93TPdJyT2U8l7JxdEvlSdAOvGAHMO/EvbFjlt3vvZK/KBrufD4wc4v56ET7 craig@ba"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDZqIeMUlW9zTxADY6M4VShlFn4a65hOpFlEaOupLt3GXzL2cIrBLnfVqo2mV6M3paerg9XsXifkS8xLnjv9Urs6+v2peePJghY8eyLrZS5UgV8fsx7el5DSU1SfSUi8NFnloHD2WkrVvJj9DBaLbWWiEtFQQucjT9uRJoxk6nOnCOe4dLmgWWdgPUAdu/1UAABtI5V2MU3cjI3D9jl+dWammF7TF/CKH6cK8p9txO/+nFyf0Y9ZWX60XpAQ+gPDVhbuB1IlD6g+NozMRNBiA23veF4k76srsSLgdpywqzJQCYGvn8flKx2pQW/MeRnjGFoUg/jMR3bCy6+OiG3zgZ5V+Io57Fma6VE0AGwWiHA+lYhyc8JWaNCoDllfeyyXAfAhCTLd8+SoXBjpNdI0fOukwPzKfZNc6/qaeAZ6J25HgOihpT+mPHW5DBrz1jKL9jlkUqe8ifICdtLMmYt2Rsu3KFAi+JCyEAkvmi0MpY2Otpjj9tSYS5UfrY9TDd3/E8= craigswank@Craigs-Mac-mini.local"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDV2gfMm9saRThs+/Z0+hK8gvyCOKB0rxwv1MUSy+IvwJ/iYXBo6ws7+091CurdfR6VbAxLYFTGPyt3aBehd48F9HGJ7LN6EARLFkw+62XEgee2YVlZ2LicBwygoLL/OLseHHW+Wx+3JPv4ZTt4ThGClHDl4PE7cMogz0/ZvjmNpbUhyNbm/4bbsXcN/dkQaSz9Bbtfb7MV5yDwvfMkV7dC4jMZoDUPMXlZ+IzSdYAqMsqdqbaHzzqGhWgbY4PWJ2pCG64WXUuaeMJSdQtyGby/Mm8gXKAoPPD94Q5vGN9BLrFkTIlEEKcjkjTy8XawWoyaCeif+ZMVe9HPrmyThDPX Generated By Termius"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC0wI0DmBLKI4uNo1a6KrNkU1I9CXZqWpsH4wBmk3AxC1mOP1iYOHcED47Fl0ZSJqR9Ea5K3EUQpTXhRQlSdThdhRnKvO+wjdtqux69ijIf53PhRNmqy5yMTWWTCxJ5aOxTR2HN/Pxe294za1W2nNdSRZmVtqlDwSvrEc4cwxJrLYfrcQS0MEwVkjnIJaFCpbihpSB5/ROvKLVKWw4iNlA3ed7//s366Wopck6aTlrhkcMaq1V0TKyP26q8EWT34h3E6i8V9Q2rV9ehlS26UK0ApW1nphF1pccnagYUBIK6IWs04PH2BgAhe8315Wj9EkjhW/JWEZxyj1+w9qKwGWbP craigswank@Craigs-MacBook-Pro.local"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCtGCJvFD4OO5d/u/kVm5pWaCSZ6s4ti3IktnK4KDAgIUH9Dh0tAk0kh5g1SYi3yiQ33CE3OpAxKbJ7U0+f4qyqT5B5D3AZ2LtX6YqitT0S0loYdipJ0/eggkUADvlIYU9M0RYra7Pb5xqXjRmxiFQTVT8Tphkt3nlRIysoERoKSJE7TYD2Wi4XmM3PzP2fO4ulV+xaVwmRydn7GXtqHE9KVDZXwUU89B5CLbpK0+u2AeZ9K2PSKA1NLMIJ/LOv7/MjabV3ZSCNkfaG2zw9RarSh48qpqNT3+V2VDDk5CoojIaUkYwBX7gZcYEWdcBicfzzBvLc1kml1A7QvWLE/O/F craigswank@Craigs-MacBook-Pro.local"
    ];
  };

  fonts = {
    fonts = with pkgs; [
      corefonts
      dejavu_fonts
      dina-font
      fira-code
      fira-code-symbols
      font-awesome-ttf
      liberation_ttf
      material-design-icons
      mplus-outline-fonts
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

    fontconfig.dpi = 192;
  };

  programs = {
    dconf.enable = true;
    nm-applet.enable = true;

    # Some programs need SUID wrappers, can be configured further or are
    # started in user sessions.
    # programs.mtr.enable = true;
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
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

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?
}
