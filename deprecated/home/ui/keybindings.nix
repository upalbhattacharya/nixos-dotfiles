{ config, lib, pkgs, ... }:
let
	modifier = config.wayland.windowManager.sway.config.modifier;
	menu = config.wayland.windowManager.sway.config.menu;
	left = "h";
	right = "l";
	up = "k";
	down = "j";
in
{
	wayland.windowManager.sway.config.keybindings = lib.mkDefault {

		# Basic
		"${modifier}+Shift+q" = "exec swaynag -t warning -m 'You pressed the exit shortcut. Do you really want to exit sway? This will end your Wayland session.' -B 'Yes, exit sway' 'swaymsg exit'";
		"${modifier}+Shift+c" = "kill";
		"${modifier}+Shift+r" = "reload";

		# Applications
		"${modifier}+Return" = "exec ${pkgs.kitty}/bin/kitty";
		"${modifier}+r" = "exec ${pkgs.fuzzel}/bin/fuzzel";
		"${modifier}+q" = "exec ${pkgs.brave}/bin/brave";
		"Ctrl+Alt+l" = "exec ${pkgs.swaylock}/bin/swaylock";
		"Ctrl+Space" = "exec ${pkgs.mako}/bin/makoctl dismiss -a";

		"${modifier}+${left}" = "focus left";
		"${modifier}+${right}" = "focus right";
		"${modifier}+${up}" = "focus up";
		"${modifier}+${down}" = "focus down";

		# Workspaces
		"${modifier}+1" = "workspace number 1";
		"${modifier}+2" = "workspace number 2";
		"${modifier}+3" = "workspace number 3";
		"${modifier}+4" = "workspace number 4";
		"${modifier}+5" = "workspace number 5";
		"${modifier}+6" = "workspace number 6";
		"${modifier}+7" = "workspace number 7";
		"${modifier}+8" = "workspace number 8";
		"${modifier}+9" = "workspace number 9";
		"${modifier}+0" = "workspace number 10";

		"${modifier}+Shift+1" = "move container to workspace number 1";
		"${modifier}+Shift+2" = "move container to workspace number 2";
		"${modifier}+Shift+3" = "move container to workspace number 3";
		"${modifier}+Shift+4" = "move container to workspace number 4";
		"${modifier}+Shift+5" = "move container to workspace number 5";
		"${modifier}+Shift+6" = "move container to workspace number 6";
		"${modifier}+Shift+7" = "move container to workspace number 7";
		"${modifier}+Shift+8" = "move container to workspace number 8";
		"${modifier}+Shift+9" = "move container to workspace number 9";
		"${modifier}+Shift+0" = "move container to workspace number 10";

		# Fullscreen
		"${modifier}+f" = "fullscreen";

		# Scratchpad
		"${modifier}+Shift+minus" = "move scratchpad";
		"${modifier}+minus" = "scratchpad show";

		# Layout
    		"${modifier}+b" = "splith";
    		"${modifier}+v" = "splitv";

    		"${modifier}+s" =  "layout stacking";
    		"${modifier}+w" =  "layout tabbed";
    		"${modifier}+e" =  "layout toggle split";

    		"${modifier}+Shift+space" = "floating toggle";
    		"${modifier}+space" = "focus mode_toggle";
    		"${modifier}+a" = "focus parent";

		# Volume
		"XF86AudioMute" = "exec wpctl set-mute @DEFAULT_SINK@ toggle";
		"XF86AudioRaiseVolume" = "exec wpctl set-volume @DEFAULT_SINK@ 5%+";
		"XF86AudioLowerVolume" = "exec wpctl set-volume @DEFAULT_SINK@ 5%-";
		"XF86AudioMicMute" = "exec wpctl set-mute @DEFAULT_SOURCE@ toggle";

		"Ctrl+Up" = "exec wpctl set-volume @DEFAULT_SINK@ 5%+";
		"Ctrl+Down" = "exec wpctl set-volume @DEFAULT_SINK@ 5%-";

		"Ctrl+m" = "exec wpctl set-mute @DEFAULT_SINK@ toggle";
		"Ctrl+Alt+m" = "exec wpctl set-mute @DEFAULT_SOURCE@ toggle";

	};
}
