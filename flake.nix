{
  description = "First flake";
  inputs = {
    nixpkgs = {
      url = "nixpkgs/nixos-unstable";
    };
    catppuccin = {
      url = "github:catppuccin/nix";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    zen-browser.url = "github:0xc000022070/zen-browser-flake";
  };
  outputs =
    {
      self,
      nixpkgs,
      home-manager,
      catppuccin,
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
          specialArgs = { inherit inputs; };
          modules = [
            ./configuration.nix
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.workboots = {
                imports = [
                  ./home.nix
                  catppuccin.homeManagerModules.catppuccin
                ];
              };
              home-manager.extraSpecialArgs = {
                inherit system;
                inherit inputs;
              };
            }
          ];
        };
      };
      # homeConfigurations = {
      # 	workboots = home-manager.lib.homeManagerConfiguration {
      # 		inherit pkgs;
      # 		modules = [
      # 			./home.nix
      #       			catppuccin.homeManagerModules.catppuccin
      # 		];
      # 	};
      # };

    };
}
