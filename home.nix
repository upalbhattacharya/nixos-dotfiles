{ config, pkgs, ... }: let
	name = "workboots";
	version = "24.05";
in
{
	imports = [
		./home
	];

	home = {
		username = "${name}";
		homeDirectory = "/home/${name}";
		stateVersion = "${version}";
		sessionVariables = {
			EDITOR = "vim";
 			# session.
          		XDG_CURRENT_DESKTOP = "sway";
          		XDG_SESSION_DESKTOP = "sway";
          		XDG_SESSION_TYPE = "wayland";

          		# wayland stuff.
          		QT_QPA_PLATFORM="wayland";
          		# QT_WAYLAND_DISABLE_WINDOWDECORATION="1";
          		SDL_VIDEODRIVER = "wayland";

          		# apps stuff.
          		_JAVA_AWT_WM_NONREPARENTING=1;

		};
		packages = [];
		file = {
		wallpaper.source = "/home/workboots/.dotfiles/catppuccin-hashtags-black.png";
		};
	};
	programs.home-manager.enable = true;

}
