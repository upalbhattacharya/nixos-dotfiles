{
  description = "First flake";
  inputs = {
    nixpkgs = {
      url = "nixpkgs/nixos-unstable";
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
          specialArgs = {
            inherit system;
            inherit inputs;
          };
          modules = [
            ./configuration.nix
          ];
        };
      };
    };
}
