{ config, lib, pkgs, ... }:
{
	imports = [
		# ./colors.nix
		./fuzzel.nix
		./kanshi.nix
		# ./keybindings.nix
		# ./sway.nix
    ./hyprland.nix
		./swaylock.nix
		./waybar.nix
		./wpaperd.nix
	];
}
