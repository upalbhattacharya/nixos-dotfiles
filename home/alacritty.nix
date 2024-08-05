{ ... }:

{

	programs.alacritty = {
		enable = true;
		settings = {
			env.TERM = "alacritty";
			window = {
				decorations = "full";
				title = "Alacritty";
				dynamic_title = true;
				class = {
					instance = "Alacritty";
					general = "Alacritty";
				};
			};
			font = {
				size = 22.00;
			};
			colors = {
				primary = {
					background = "#1d1f21";
					foreground = "#c5c8c6";
				};
			};
		};
	};
}
