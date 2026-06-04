# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./i3.nix
      # Uncomment after first boot, once the home-manager channel is added, then run:
      # sudo nix-channel --add https://github.com/nix-community/home-manager/archive/release-26.05.tar.gz home-manager
      # sudo nix-channel --update
      <home-manager/nixos>
    ];

  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = true;
  home-manager.users.craig = import /home/craig/Projects/dotfiles/nix/workvm/home.nix;

  services.xserver = {
    enable = true;
    dpi = 192;
    windowManager.i3.enable = true;
    windowManager.i3.configFile = "/etc/i3.conf";
    displayManager.lightdm.enable = true;
    displayManager.autoLogin = {
      enable = true;
      user = "craig";
    };
    # Fixed-resolution fallback. Only matters when the UTM display card does
    # not negotiate a mode on its own (e.g. plain "virtio-ramfb"). With
    # "virtio-gpu-pci" + spice-vdagent the desktop auto-resizes and this is a
    # harmless no-op. Adds the mode if the card doesn't already advertise it,
    # then switches to it. Change WIDTHxHEIGHT to taste.
    displayManager.sessionCommands = ''
      ${pkgs.xorg.xrandr}/bin/xrandr -s 1920x1080 2>/dev/null || {
        out=$(${pkgs.xorg.xrandr}/bin/xrandr | ${pkgs.gnused}/bin/sed -n 's/ connected.*//p' | head -n1)
        mode=$(${pkgs.xorg.cvt}/bin/cvt 1920 1080 | ${pkgs.gnused}/bin/sed -n '2s/^Modeline "\([^"]*\)" \(.*\)/\1 \2/p')
        name=$(echo "$mode" | ${pkgs.gawk}/bin/awk '{print $1}')
        ${pkgs.xorg.xrandr}/bin/xrandr --newmode $mode 2>/dev/null || true
        ${pkgs.xorg.xrandr}/bin/xrandr --addmode "$out" "$name" 2>/dev/null || true
        ${pkgs.xorg.xrandr}/bin/xrandr --output "$out" --mode "$name" 2>/dev/null || true
      }
    '';
    xkb.layout = "us";
  };

  # Guest agents so the UTM/QEMU SPICE display can negotiate and
  # auto-resize the resolution to match the window. Requires the VM's
  # "Emulated Display Card" in UTM to be set to "virtio-ramfb-gl (GPU Supported)".
  services.qemuGuest.enable = true;
  services.spice-vdagentd.enable = true;

  environment.variables = {
    GDK_SCALE = "2";
    GDK_DPI_SCALE = "1";
    QT_AUTO_SCREEN_SCALE_FACTOR = "1";
    _JAVA_OPTIONS = "-Dsun.java2d.uiScale=2";
  };

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "yavin"; # Define your hostname.
  time.timeZone = "America/Denver";
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
    #useXkbConfig = true; # use xkbOptions in tty.
  };

  users.users.craig = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQC6lfhGGP3P8JJJZOG90dPBQImsYRYkbW+Bnb6gvczZB1Y8bcfOeY7Xxw5zwZaNjRpFExVsvIxPUEjQE2+bXFIHqdv0Ai0Qrtl6inYI82jnHf3j6rXF/GjYenVqkabtFMlCpyabyTKKVC4/fX1qcewsVR/FI/c3oUmze/CK4V8Rlrb/tsjptQYHVIr5NUOHyVObOQDXgsh+P9Bo5fqtRflt2lSFAbEDbqGEXixovZbEpBVbvbWbtc5o9pQ35QS4qh+1aSlBMbnlhMXjNFO0X+3HO44QtZRp3oLINTx6A15o232hfBjDCKw5NGF82PdP1VmtOTAkSOZKeUlD7fxNI6zwHpHLS7ybBr1sC9G4SbxqRCHb74COBT9KEkYRG5poI0BUhk3d/ZBSIm53EkSP11+gfq3N0MxGems9xry0HQBo/urvHs6yb8ImDJaOL9LW9kC2YfvwnQhCyxfnNjxDpZ2NfV37nh2BjSFiGC0wVgVqEOqClNrmDSRdaNHerh7Ke0U= craig@Craigs-MacBook-Pro.local"
     ];
  };

  environment.systemPackages = with pkgs; [
    git
  ];

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Copy the NixOS configuration file and link it from the resulting system
  # (/run/current-system/configuration.nix). This is useful in case you
  # accidentally delete configuration.nix.
  # system.copySystemConfiguration = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "26.05"; # Did you read the comment?
}

