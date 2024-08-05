{ config, pkgs, ... }: let
	name = "workboots";
	version = "24.05";
in
{
	imports = [
		./wayland
		./sh.nix
		./alacritty.nix
		./brave.nix
		./version-control.nix
		./kanshi.nix
	];

}
