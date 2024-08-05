{ ... }: let
	wallpaper = "/home/workboots/.dotfiles/assets/wallpapers";
in {
	programs.wpaperd = {
		enable = true;
		settings = {
			default = {
				path = "${wallpaper}";
			};
		};
	};
}
