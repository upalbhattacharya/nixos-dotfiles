{ config, lib, pkgs, ... }:

{
	programs.fuzzel = {
		enable = true;
		settings = {
			main = {
				terminal = "${pkgs.kitty}/bin/kitty";
				layer = "overlay";

				font = "Inconsolata Nerd Font Mono:size=18";
			};
			colors = {
				background="1e1e2edd";
				text="cdd6f4ff";
				match="eba0acff";
				selection = "585b70ff";
				selection-match = "eba0acff";
				selection-text = "cdd6f4ff";
				border = "b4befeff";
			};
		};
	};
}
