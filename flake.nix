{
  description = "First flake";
  inputs = {
    nixpkgs = {
      url = "nixpkgs/nixos-unstable";
    };
    zen-browser = {
      url = "github:0xc000022070/zen-browser-flake";
    };
  };
  outputs =
    {
      self,
      nixpkgs,
      ...
    }@inputs:
    let
      lib = nixpkgs.lib;
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      nixosConfigurations = {
        nixos-workboots = lib.nixosSystem {
          inherit system;
          specialArgs = { inherit inputs; };
          modules = [
            ./configuration.nix
          ];
        };
      };
    };
}
