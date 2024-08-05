{ config, lib, pkgs, ... }:
{
	imports = [
		./keybindings.nix
	];
	wayland.windowManager.sway = {
		enable = true;
		extraOptions = [
			"--unsupported-gpu"
		];
		wrapperFeatures.gtk = true;
		config = {
			# Modifier
			modifier = "Mod4";

			# Vim Navigation
			left = "h";
			right = "l";
			up = "k";
			down = "j";
			terminal = "alacritty";
			## menu = "wofi --show run";

			# Fonts
			fonts = {	
				names = [ "Inconsolata Nerd Font Mono" ];
				size = 12.0;
			};

			# Status bar(s)
			bars = [{
				fonts.size = 15.0;
				# command = "waybar"; You can change it if you want
				position = "top";
			}];

			# Window
			window = {
				border = 5;
				titlebar = false;
			};

			# Gaps
			gaps = {
				smartGaps = true;
				smartBorders = "on";
				inner = 10;
			};

			# Startup
			startup = [
				{
					command = "autotiling";
					always = true;
				}
			];
		};
	};
}
