{
  description = "A flake containing the nixos configurations of most of my personal systems.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:nixos/nixos-hardware";
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    wayland-overlay = {
      url = "github:nix-community/nixpkgs-wayland";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    emacs-overlay,
    home-manager,
    nixos-hardware,
    wayland-overlay,
    pre-commit-hooks,
    flake-utils,
    ...
  } @ inputs:
    with flake-utils.lib;
      eachSystem
      [system.x86_64-linux system.aarch64-linux]
      (sys: let
        overlays = [emacs-overlay.overlay wayland-overlay.overlay];
        pkgs = nixpkgs.legacyPackages.${sys};
      in {
        packages = rec {
          hello = pkgs.writeShellApplication {
            name = "helloDotfiles";
            runtimeInputs = [pkgs.coreutils];
            text = ''
              printf "\n\n"
              echo 👋👋 hello from ~averagechris/dotfiles
              echo have a nice day 😎
              printf "\n\n"
            '';
          };
          default = hello;
        };

        checks =
          // (
            if (sys == system.x86_64-linux)
            then {
              # these checks take ~4GB of memory right now to run
              # since nix flake check loads all of outputs.nixosConfigurations
              # into memory at once 😢
              # thelio-nixos = self.outputs.nixosConfigurations.thelio-nixos.config.system.build.toplevel;
              # xps-nixos = self.outputs.nixosConfigurations.xps-nixos.config.system.build.toplevel;
              # tootsie = self.outputs.nixosConfigurations.tootsie.config.system.build.toplevel;
              # taz = self.outputs.nixosConfigurations.taz.config.system.build.toplevel;
            }
            else {}
          )
          // (
            if (sys == system.aarch64-linux)
            then {
              work-vm = self.outputs.nixosConfigurations.work-vm.config.system.build.toplevel;
            }
            else {}
          );
      })
      // rec {
        overlays = {
          emacs = emacs-overlay.overlay;
          wayland = wayland-overlay.overlay;
        };
        nixosConfigurations = {
          work-vm = nixpkgs.lib.nixosSystem {
            system = system.aarch64-linux;
            specialArgs = {
              inherit (self.outputs) overlays;
              inherit inputs;
            };
            modules = [
              ./nixpkgs/nixos/work-vm
              ./nixpkgs/nixos/common.nix
              ./nixpkgs/nixos/graphical.nix
              ./nixpkgs/nixos/greetd.nix
              ./nixpkgs/nixos/users/craig.nix
              home-manager.nixosModules.home-manager
              {
                home-manager.useGlobalPkgs = true;
                home-manager.useUserPackages = true;
              }
            ];
          };
        };
      };
}
