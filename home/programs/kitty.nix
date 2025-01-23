{ pkgs, ... }:

{
	programs.kitty = {
		enable = true;
		themeFile = "Catppuccin-Mocha";
		extraConfig = ''
		font_family Inconsolata Nerd Font Mono
		font_size 18
		'';
	};
}
