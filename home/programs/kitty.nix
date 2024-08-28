{ pkgs, ... }:

{
	programs.kitty = {
		enable = true;
		theme = "Catppuccin-Mocha";
		extraConfig = ''
		font_family Inconsolata Nerd Font Mono
		font_size 24
		'';
	};
}
