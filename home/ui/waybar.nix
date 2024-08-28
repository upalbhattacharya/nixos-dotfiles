{ config, lib, pkgs, ... }:

{
	programs.waybar = {
		enable = true;
		settings = {
			mainBar = {
				layer = "top";
				position = "top";
				height = 28;
				# width = 1280;
				# spacing = 4;
				fixed-center = false;
				reload_style_on_change = true;

	  		# Choose the order of the modules
	  		modules-left = [
	    		"sway/workspaces"
	  		];
	  		modules-center = [
	    		"sway/window"
	  		];
	  		modules-right = [
	    		"tray"
	    		"custom/separatorleft"
	    		"pulseaudio"
	    		"custom/separatorleft2"
	    		"custom/memory"
	    		"custom/separatorleft3"
	    		"cpu"
	    		"custom/separatorleft4"
	    		"temperature"
	    		"custom/separatorleft5"
	    		"battery"
	    		"custom/separatorleft6"
	    		"network"
	    		"custom/separatorleft7"
	    		"backlight"
	    		"custom/separatorleft8"
	    		"custom/clock"
	  	];
	  	# Modules configuration
	  	"sway/workspaces" = {
	  		all-outputs = false;
	  		disable-scroll = true;
	  		format = "{icon}";
			};
	  	"sway/window" = {
	    	separate-outputs = true;
	  	};
	  	"tray" = {
	    	# "icon-size" = 21;
	    	spacing = 10;
	  	};
	  	"custom/clock" = {
	    	interval = 1;
	    	format = "{}";
	    	exec = pkgs.writeShellScript "clock" ''
					date +"%Y-%m-%d %T"
				'';
	  	};
	  	"cpu" = {
	    	interval = 10;
	    	format = "CPU: {usage:3}%";
	    	tooltip =  false;
	  	};
	  	"temperature" = {
	    	thermal-zone = 0;
	    	critical-threshold = 100;
	    	format = "TEMP: {temperatureC}°C";
	  	};
	  	"backlight" = {
	    	device = "acpi_video1";
	    	format = "BCKL: {percent:3}%";
	  	};
	  	"battery" = {
	    	format = "BAT: {capacity}%";
	    	format-charging = "CHG: {capacity}%";
	    	format-plugged = "PWR: {capacity}%";
	  	};
	  	"network" = {
	    	format-wifi = "NET: {signalStrength:3}%";
	    	format-ethernet = "NET:  eth";
	    	format-disconnected = "NET: Disc";
	  	};
	  	"pulseaudio" = {
	    	format = "VOL: {volume:3}%";
	    	format-muted = "VOL: MUTE";
	  	};
	  	"custom/memory" = {
	    	interval = 10;
	    	format = "MEM: {}";
	    	exec = pkgs.writeShellScript "memory" ''
				free -h | awk 'FNR == 2 {print $6}'
				'';
	  	};
	  	"custom/separatorleft" = {
	    	format = "";
	  	};
	  	"custom/separatorleft2" = {
	    	format = "";
	  	};
	  	"custom/separatorleft3" = {
	    	format = "";
	  	};
	  	"custom/separatorleft4" = {
	    	format = "";
	  	};
	  	"custom/separatorleft5" = {
	    	format = "";
	  	};
	  	"custom/separatorleft6" = {
	    	format = "";
	  	};
	  	"custom/separatorleft7" = {
	    	format = "";
	  	};
	  	"custom/separatorleft8" = {
	    	format = "";
	  	};
		};
	};
	style = ''
	@define-color base   #1e1e2e;
	@define-color mantle #181825;
	@define-color crust  #11111b;
	
	@define-color text     #cdd6f4;
	@define-color subtext0 #a6adc8;
	@define-color subtext1 #bac2de;
	
	@define-color surface0 #313244;
	@define-color surface1 #45475a;
	@define-color surface2 #585b70;
	
	@define-color overlay0 #6c7086;
	@define-color overlay1 #7f849c;
	@define-color overlay2 #9399b2;
	
	@define-color blue      #89b4fa;
	@define-color lavender  #b4befe;
	@define-color sapphire  #74c7ec;
	@define-color sky       #89dceb;
	@define-color teal      #94e2d5;
	@define-color green     #a6e3a1;
	@define-color yellow    #f9e2af;
	@define-color peach     #fab387;
	@define-color maroon    #eba0ac;
	@define-color red       #f38ba8;
	@define-color mauve     #cba6f7;
	@define-color pink      #f5c2e7;
	@define-color flamingo  #f2cdcd;
	@define-color rosewater #f5e0dc;
	* {
	  border: none;
	  border-radius: 0;
	  font-family: Inconsolata Nerd Font Mono;
	  font-weight: 600;
	  font-size: 22px;
	  min-height: 0;
	}
	
	#window {
	  padding-left: 45px;
	  padding-right: 45px;
	}
	
	window#waybar {
	  background-color: @mantle;
	  padding-top: 4px;
	  padding-bottom: 4px;
	  padding-left: 4px;
	  padding-right: 4px;
	  color: @text;
	  transition-property: background-color;
	  transition-duration: 0.5s;
	}
	
	window#waybar.hidden {
	  opacity: 0.2;
	}
	
	/*
	window#waybar.empty {
	    background-color: transparent;
	}
	window#waybar.solo {
	    background-color: #FFFFFF;
	}
	*/
	
	#workspaces button {
	  background-color: @mantle;
	  color: @text;
	  font-weight: 600;
	}
	
	#workspaces button.focused {
	  background-color: @surface0;
	  color: @green;
	}
	#workspaces button.urgent {
	  background-color: @red;
	  color: @mantle;
	}
	
	#window,
	#pulseaudio,
	#custom-memory,
	#cpu,
	#temperature,
	#battery,
	#network,
	#backlight,
	#custom-clock {
	  padding-left: 8px;
	  padding-right: 8px;
	  border-left: inset;
	}
	
	/* #window { */
	/*   background-color: @surface0; */
	/*   color: @text; */
	/* } */
	
	#tray {
	  padding-left: 8px;
	  padding-right: 8px;
	  min-width: 40px;
	}
	
	#pulseaudio {
	  background-color: @rosewater;
	  color: @mantle;
	}
	
	#custom-memory {
	  background-color: @pink;
	  color: @mantle;
	}
	
	#cpu {
	  background-color: @mauve;
	  color: @mantle;
	}
	#temperature {
	  background-color: @red;
	  color: @mantle;
	}
	#battery {
	  background-color: @peach;
	  color: @mantle;
	}
	#network {
	  background-color: @yellow;
	  color: @mantle;
	}
	#backlight {
	  background-color: @green;
	  color: @mantle;
	}
	#custom-clock {
	  background-color: @lavender;
	  color: @mantle;
	}
	#custom-separatorleft {
	  font-size: 30px;
	  background-color: transparent;
	  color: @rosewater;
	}
	#custom-separatorleft2 {
	  font-size: 30px;
	  background-color: @rosewater;
	  color: @pink;
	}
	#custom-separatorleft3 {
	  font-size: 30px;
	  background-color: @pink;
	  color: @mauve;
	}
	#custom-separatorleft4 {
	  font-size: 30px;
	  background-color: @mauve;
	  color: @red;
	}
	#custom-separatorleft5 {
	  font-size: 30px;
	  background-color: @red;
	  color: @peach;
	}
	#custom-separatorleft6 {
	  font-size: 30px;
	  background-color: @peach;
	  color: @yellow;
	}
	#custom-separatorleft7 {
	  font-size: 30px;
	  background-color: @yellow;
	  color: @green;
	}
	#custom-separatorleft8 {
	  font-size: 30px;
	  background-color: @green;
	  color: @lavender;
	}
	@keyframes scroll {
	  from {
	    background-position: -33.941125497px 0;
	  }
	  to {
	    background-position: 0 0;
	  }
	}
	'';
	};
}
