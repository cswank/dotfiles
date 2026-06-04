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

  # Allow unfree packages. With home-manager.useGlobalPkgs = true, home
  # packages use this system-level nixpkgs config (a nixpkgs.config in
  # home.nix would be ignored).
  nixpkgs.config.allowUnfree = true;

  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = true;
  # Move pre-existing, unmanaged files (e.g. a ~/.zshrc written by the
  # oh-my-zsh installer) to <name>.backup instead of failing activation.
  home-manager.backupFileExtension = "backup";
  home-manager.users.craig = import /home/craig/Projects/dotfiles/nix/workvm/home.nix;

  services.xserver = {
    enable = true;
    dpi = 220;
    windowManager.i3.enable = true;
    windowManager.i3.configFile = "/etc/i3.conf";
    displayManager.lightdm.enable = true;
    # Resolution is left to spice-vdagent, which resizes the guest to match
    # the UTM window. No fixed xrandr pin: a hard-coded mode breaks when you
    # reconnect/switch displays (guest stays at the old size, so the fixed
    # dpi makes everything look giant).
    #
    # But a runtime resize makes the X server recompute its DPI from the
    # reported physical size, overriding services.xserver.dpi (at 4K that
    # comes out low -> everything tiny). Pin Xft.dpi as an X resource instead:
    # GTK/Firefox/terminal and i3's pango fonts honor it, and a resize doesn't
    # touch it. This keeps UI sizing constant while resolution stays dynamic.
    displayManager.sessionCommands = ''
      echo "Xft.dpi: 220" | ${pkgs.xorg.xrdb}/bin/xrdb -merge
    '';
    xkb.layout = "us";
  };

  services.displayManager.autoLogin = {
    enable = true;
    user = "craig";
  };

  # Guest agents so the UTM/QEMU SPICE display can negotiate and
  # auto-resize the resolution to match the window. Set the VM's
  # "Emulated Display Card" in UTM to "virtio-gpu-pci" (the "-gl" variants
  # can crash UTM's host-side renderer on Apple Silicon).
  services.qemuGuest.enable = true;
  services.spice-vdagentd.enable = true;

  environment.variables = {
    GDK_SCALE = "1";
    GDK_DPI_SCALE = "1";
    QT_AUTO_SCREEN_SCALE_FACTOR = "1";
    _JAVA_OPTIONS = "-Dsun.java2d.uiScale=1";
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

  # Use zsh as the login shell (gnome-terminal launches the login shell,
  # so this is what makes ~/.zshrc get read). programs.zsh.enable adds zsh
  # to /etc/shells and sets up the system zsh integration.
  programs.zsh.enable = true;

  users.users.craig = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
    shell = pkgs.zsh;
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQC6lfhGGP3P8JJJZOG90dPBQImsYRYkbW+Bnb6gvczZB1Y8bcfOeY7Xxw5zwZaNjRpFExVsvIxPUEjQE2+bXFIHqdv0Ai0Qrtl6inYI82jnHf3j6rXF/GjYenVqkabtFMlCpyabyTKKVC4/fX1qcewsVR/FI/c3oUmze/CK4V8Rlrb/tsjptQYHVIr5NUOHyVObOQDXgsh+P9Bo5fqtRflt2lSFAbEDbqGEXixovZbEpBVbvbWbtc5o9pQ35QS4qh+1aSlBMbnlhMXjNFO0X+3HO44QtZRp3oLINTx6A15o232hfBjDCKw5NGF82PdP1VmtOTAkSOZKeUlD7fxNI6zwHpHLS7ybBr1sC9G4SbxqRCHb74COBT9KEkYRG5poI0BUhk3d/ZBSIm53EkSP11+gfq3N0MxGems9xry0HQBo/urvHs6yb8ImDJaOL9LW9kC2YfvwnQhCyxfnNjxDpZ2NfV37nh2BjSFiGC0wVgVqEOqClNrmDSRdaNHerh7Ke0U= craig@Craigs-MacBook-Pro.local"
     ];
  };

  environment.systemPackages = with pkgs; [
    git
  ];

  fonts.packages = with pkgs; [
    nerd-fonts.fira-code
  ];

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Resolve .local (mDNS) hostnames advertised by avahi on the LAN.
  # nssmdns4 hooks mDNS into nsswitch so getaddrinfo() resolves *.local;
  # openFirewall lets mDNS (UDP 5353) through the default firewall.
  services.avahi = {
    enable = true;
    nssmdns4 = true;
    openFirewall = true;
  };

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

