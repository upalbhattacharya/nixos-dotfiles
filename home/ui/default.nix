{ config, lib, pkgs, ... }:
{
	imports = [
		./kanshi.nix
		./keybindings.nix
		./sway.nix
		./tofi.nix
		./waybar.nix
		./wpaperd.nix
	];
}
