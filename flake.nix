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
	};
	outputs = {self, nixpkgs, home-manager, catppuccin, ...}:
	let
		lib = nixpkgs.lib;
		system = "x86_64-linux";
		pkgs = nixpkgs.legacyPackages.${system};
	in {
		nixosConfigurations = {
			nixos-workboots = lib.nixosSystem {
				inherit system;
				modules = [
					./configuration.nix
				];
			};
		};
		homeConfigurations = {
			workboots = home-manager.lib.homeManagerConfiguration {
				inherit pkgs;
				modules = [
					./home.nix
	        			catppuccin.homeManagerModules.catppuccin
				];
			};
		};

	};
}
