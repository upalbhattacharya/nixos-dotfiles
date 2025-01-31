{ config, lib, pkgs, ... }:

{
	xsession.windowManager.i3 = {
		enable = true;
		# extraOptions = [
		#		"--unsupported-gpu"
		# ];
		# wrapperFeatures.gtk = true;
		config = {
			# Modifier
			modifier = "Mod4";

			# Vim Navigation
			# left = "h";
			# right = "l";
			# up = "k";
			# down = "j";
			terminal = "kitty";
			menu = "fuzzel";

			# Fonts
			fonts = {	
				names = [ "Inconsolata Nerd Font Mono" ];
				size = 18.0;
			};

			# Status bar(s)
			# bars = [{
		 	#		command = "waybar";
			# }];

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
					command = "${pkgs.autotiling}/bin/autotiling";
					always = true;
				}
				# {
			# 		command = "${pkgs.wpaperd}/bin/wpaperd";
			# 		always = true;
			# 	}
				{
					command = "systemctl --user restart kanshi.service";
					always = true;
				}
				{
					command = "${pkgs.mako}/bin/mako";
					always = true;	
				}
			];
		};
	};
}
