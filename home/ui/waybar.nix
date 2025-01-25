{
  config,
  lib,
  pkgs,
  ...
}:

{
  programs.waybar = {
    enable = true;
    settings = {
      mainBar = {
        layer = "top";
        position = "top";
        height = 24;
        # width = 1280;
        # spacing = 4;
        fixed-center = false;
        reload_style_on_change = true;

        # Choose the order of the modules
        modules-left = [
          "hyprland/workspaces"
        ];
        modules-center = [
          "hyprland/window"
        ];
        modules-right = [
          "tray"
          "custom/separator"
          "pulseaudio"
          "custom/separator"
          "custom/memory"
          "custom/separator"
          "cpu"
          "custom/separator"
          "temperature"
          "custom/separator"
          "battery"
          "custom/separator"
          "network"
          "custom/separator"
          "backlight"
          "custom/separator"
          "custom/clock"
        ];
        # Modules configuration
        "hyprland/workspaces" = {
          all-outputs = false;
          disable-scroll = true;
          format = "{icon}";
        };
        "hyprland/window" = {
          separate-outputs = true;
        };
        "tray" = {
          # "icon-size" = 21;
          spacing = 10;
        };
        "custom/clock" = {
          interval = 1;
          format = "{} ";
          exec = pkgs.writeShellScript "clock" ''
            					date +"%T"
            				'';
        };
        "cpu" = {
          interval = 10;
          format = "{icon} {usage:3}%";
          format-icons = [ "" ];
          tooltip = false;
        };
        "temperature" = {
          thermal-zone = 0;
          interval = 2;
          critical-threshold = 100;
          format = "{icon} {temperatureC}°C";
          format-icons = [ "" ];
        };
        "backlight" = {
          device = "acpi_video1";
          format = "{icon} {percent:3}%";
          format-icons = [ "󰃠" ];
        };
        "battery" = {
          format = "{icon} {capacity}%";
          interval = 5;
          states = {
            warning = 15;
            critical = 10;
          };
          format-icons = {
            default = [ "󰂁" ];
            charging = [ "󱐋" ];
            discharging = [ "󰂁" ];
            plugged = [ "" ];
            full = [ "" ];
          };
        };
        "network" = {
          format-wifi = "{icon} {signalStrength:3}%";
          format-ethernet = "{icon}  eth";
          format-disconnected = "{icon} Disc";
          format-icons = {
            wifi = [ "󰖩" ];
            ethernet = [ "󰌗" ];
            disconnected = [ "󰖪" ];
          };
        };
        "pulseaudio" = {
          format = "{icon} {volume:3}%";
          format-muted = "{icon} MUTE";
          format-icons = [ "" ];
        };
        "custom/memory" = {
          interval = 10;
          format = "{icon} {}";
          format-icons = [ "" ];
          exec = pkgs.writeShellScript "memory" ''
            				free -h | awk 'FNR == 2 {print $6}'
            				'';
        };
        "custom/separator" = {
          "format" = "|";
          "interval" = "once";
          "tooltip" = false;
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
      	  font-size: 20px;
      	  min-height: 0;
      	}
      	
      	#window {
      	  padding-left: 20px;
      	  padding-right: 20px;
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
      	#workspaces button.active {
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
      	  padding-left: 4px;
      	  padding-right: 4px;
      	  border-left: inset;
      	}
      	
      	/* #window { */
      	/*   background-color: @surface0; */
      	/*   color: @text; */
      	/* } */
      	
      	#tray {
      	  padding-left: 4px;
      	  padding-right: 4px;
      	  min-width: 40px;
      	}

      	#pulseaudio {
      	  color: @rosewater;
      	  background-color: @mantle;
      	}
      	
      	#custom-memory {
      	  color: @pink;
      	  background-color: @mantle;
      	}
      	
      	#cpu {
      	  color: @mauve;
      	  background-color: @mantle;
      	}
      	#temperature {
      	  color: @red;
      	  background-color: @mantle;
      	}
      	#battery {
      	  color: @peach;
      	  background-color: @mantle;
      	}
      	#battery.discharging.warning {
      	  background-color: @peach;
      	  color: @mantle;
      	}
      	#battery.discharging.critical {
      	  background-color: @red;
      	  color: @mantle;
      	}
      	#network {
      	  color: @yellow;
      	  background-color: @mantle;
      	}
      	#backlight {
      	  color: @green;
      	  background-color: @mantle;
      	}
      	#custom-clock {
      	  color: @lavender;
      	  background-color: @mantle;
      	}
        #custom-separator {
          color: @surface1;
          margin: 0 5px;
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
