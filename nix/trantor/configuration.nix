# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  
  nixpkgs.config.allowUnfree = true;

  networking.hostName = "trantor"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Set your time zone.
  time.timeZone = "America/Denver";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.useNetworkd = true;
  networking.interfaces.enp2s0.useDHCP = true;
  networking.interfaces.wlp3s0.useDHCP = false;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };

  # Enable the X11 windowing system.
  services.xserver.enable = true;


  # Enable the Plasma 5 Desktop Environment.
  # services.xserver.displayManager.sddm.enable = true;
  # services.xserver.desktopManager.plasma5.enable = true;
  services.xserver.desktopManager.pantheon.enable = true;  
  services.xserver.displayManager.lightdm.greeters.pantheon.enable = false;
  services.xserver.displayManager.autoLogin.enable = true;
  services.xserver.displayManager.autoLogin.user = "craig";

  # Configure keymap in X11
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable CUPS to print documents.
  services.printing.enable = true;
  services.printing.browsing = true;
  services.printing.drivers = [ pkgs.brlaser ];

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.configFile = pkgs.runCommand "default.pa" {} ''
    sed 's/module-udev-detect$/module-udev-detect tsched=0/' \
    ${pkgs.pulseaudio}/etc/pulse/default.pa > $out
  '';

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.craig = {
    isNormalUser = true;
    shell = pkgs.zsh;
    extraGroups = [ "wheel" "audio" ]; # Enable ‘sudo’ for the user.
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDQbVm/bXNum2iknQeV0vEcDc/SkHXImMTQvAKmtMDYHkgmRoBswExG34B4qc8MMdbGDfyOLngVHTcSC8KVv2VP90YY6l+uNKxsBHt5KX4I7OycaDPYUwFdMEAsenWHPn5GMtM5lXlAX8BcpZSmoU99fm7KJjgfkoI/wWAsxX5D8ZhYQmP1SnrsaiPTGNUtfEnkAFedBax9jStwyGPTV1WGc/EjchZL9Ryu7myZFE8R6bEicF+VlEuk1XlH+9wa0lH/znsqwr7jIOs7TxqfQVVLFWrwE93TPdJyT2U8l7JxdEvlSdAOvGAHMO/EvbFjlt3vvZK/KBrufD4wc4v56ET7 craig@ba"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDZqIeMUlW9zTxADY6M4VShlFn4a65hOpFlEaOupLt3GXzL2cIrBLnfVqo2mV6M3paerg9XsXifkS8xLnjv9Urs6+v2peePJghY8eyLrZS5UgV8fsx7el5DSU1SfSUi8NFnloHD2WkrVvJj9DBaLbWWiEtFQQucjT9uRJoxk6nOnCOe4dLmgWWdgPUAdu/1UAABtI5V2MU3cjI3D9jl+dWammF7TF/CKH6cK8p9txO/+nFyf0Y9ZWX60XpAQ+gPDVhbuB1IlD6g+NozMRNBiA23veF4k76srsSLgdpywqzJQCYGvn8flKx2pQW/MeRnjGFoUg/jMR3bCy6+OiG3zgZ5V+Io57Fma6VE0AGwWiHA+lYhyc8JWaNCoDllfeyyXAfAhCTLd8+SoXBjpNdI0fOukwPzKfZNc6/qaeAZ6J25HgOihpT+mPHW5DBrz1jKL9jlkUqe8ifICdtLMmYt2Rsu3KFAi+JCyEAkvmi0MpY2Otpjj9tSYS5UfrY9TDd3/E8= craigswank@Craigs-Mac-mini.local"
    ];
  };

  # local packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
  #   vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    avahi
    nssmdns
    mosh
    cifs-utils
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

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

  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_13.withPackages (p: [ p.postgis ]);
    dataDir = "/mnt/postgresql/13";
  };

  system.nssModules = with pkgs.lib; optional (!config.services.avahi.nssmdns) pkgs.nssmdns;
  system.nssDatabases.hosts = with pkgs.lib; optionals (!config.services.avahi.nssmdns) (mkMerge [
    (mkOrder 900 [ "mdns4_minimal [NOTFOUND=return]" ]) # must be before resolve
    (mkOrder 1501 [ "mdns4" ]) # 1501 to ensure it's after dns
  ]);

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.openssh.passwordAuthentication = false;

  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [ 22 ];
  # 5353 for Avahi
  networking.firewall.allowedUDPPorts = [ 5353 ];
  networking.firewall.allowedUDPPortRanges = [
    { from = 60000; to = 61000; }
  ]; 
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

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

