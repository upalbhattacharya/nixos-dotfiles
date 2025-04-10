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
      zen-browser,
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
