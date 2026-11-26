{
  description = "NixOS + home-manager configuration for trantor";

  inputs = {
    # flake.lock pins the exact revisions. It was seeded with the commits the
    # old channels were on (2026-10-20) so the move to flakes was a no-op.
    # Bump with `nix flake update` or `nix flake lock --update-input <name>`.
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/release-23.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # Not following nixpkgs: sops-install-secrets needs a newer Go than 23.11
    # has, so home.nix uses the package sops-nix builds with its own nixpkgs.
    sops-nix.url = "github:Mic92/sops-nix";
  };

  outputs = { nixpkgs, home-manager, ... }@inputs: {
    nixosConfigurations.trantor = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = { inherit inputs; };
      modules = [
        ./configuration.nix
        home-manager.nixosModules.home-manager
        {
          home-manager = {
            useGlobalPkgs = true;
            extraSpecialArgs = { inherit inputs; };
            users.craig = import ./home.nix;
          };
        }
      ];
    };
  };
}
