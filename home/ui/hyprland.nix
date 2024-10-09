{ config, lib, pkgs, ... }:
let
  modifier = "SUPER";
in
{
  wayland.windowManager.hyprland = {
    enable = true;
    xwayland.enable = true;
    systemd.enable = true;
    catppuccin.enable = true;
    settings = {
      general = {
        border_size = 5;
      };
      decoration = {
        rounding = 2;
      };
    };
    extraConfig = ''
    dwindle {
      no_gaps_when_only = 1
    }
    exec-once = killall waybar; sleep 2 && ${pkgs.waybar}/bin/waybar
    exec-once = killall wpaperd; sleep 2 && wpaperd -d
    exec-once = systemctl --user restart kanshi.service
    exec-once = hypridle
    exec-once = emacs --daemon

    bind = ${modifier},Return,exec,${pkgs.kitty}/bin/kitty
    bind = ${modifier},r,exec,${pkgs.fuzzel}/bin/fuzzel
    bind = ${modifier},q,exec,${pkgs.brave}/bin/brave
    bind = ${modifier},e,exec,${pkgs.emacs}/bin/emacs
    bindr = Control,SPACE,exec,${pkgs.mako}/bin/makoctl dismiss -a
    bindr = Control&Alt,l,exec,${pkgs.hyprlock}/bin/hyprlock

    bind = ${modifier}_SHIFT,c,exec,${pkgs.hyprland}/bin/hyprctl dispatch killactive
    bind = ${modifier},m,exec,${pkgs.hyprland}/bin/hyprctl dispatch fullscreen 1
    bind = ${modifier}_SHIFT,m,exec,${pkgs.hyprland}/bin/hyprctl dispatch fullscreen 0
    bind = ${modifier},f,exec,${pkgs.hyprland}/bin/hyprctl dispatch togglefloating
    bind = ${modifier},f,exec,${pkgs.hyprland}/bin/hyprctl dispatch resizeactive exact 1280 720
    bind = ${modifier},f,exec,${pkgs.hyprland}/bin/hyprctl dispatch moveactive exact 0 0
    bind = ${modifier},s,exec,${pkgs.hyprland}/bin/hyprctl dispatch pin

    bind = ${modifier},h,exec,${pkgs.hyprland}/bin/hyprctl dispatch movefocus l
    bind = ${modifier},l,exec,${pkgs.hyprland}/bin/hyprctl dispatch movefocus r
    bind = ${modifier},j,exec,${pkgs.hyprland}/bin/hyprctl dispatch movefocus d
    bind = ${modifier},k,exec,${pkgs.hyprland}/bin/hyprctl dispatch movefocus u

    bindm = ${modifier},mouse:272,movewindow
    bindm = ${modifier},mouse:273,resizewindow

    bind = ${modifier}_SHIFT,h,exec,${pkgs.hyprland}/bin/hyprctl dispatch movewindow l
    bind = ${modifier}_SHIFT,l,exec,${pkgs.hyprland}/bin/hyprctl dispatch movewindow r
    bind = ${modifier}_SHIFT,j,exec,${pkgs.hyprland}/bin/hyprctl dispatch movewindow d
    bind = ${modifier}_SHIFT,k,exec,${pkgs.hyprland}/bin/hyprctl dispatch movewindow u

    bind = ${modifier}_SHIFT,1,exec,${pkgs.hyprland}/bin/hyprctl dispatch movetoworkspace 1
    bind = ${modifier}_SHIFT,2,exec,${pkgs.hyprland}/bin/hyprctl dispatch movetoworkspace 2
    bind = ${modifier}_SHIFT,3,exec,${pkgs.hyprland}/bin/hyprctl dispatch movetoworkspace 3
    bind = ${modifier}_SHIFT,4,exec,${pkgs.hyprland}/bin/hyprctl dispatch movetoworkspace 4
    bind = ${modifier}_SHIFT,5,exec,${pkgs.hyprland}/bin/hyprctl dispatch movetoworkspace 5
    bind = ${modifier}_SHIFT,6,exec,${pkgs.hyprland}/bin/hyprctl dispatch movetoworkspace 6
    bind = ${modifier}_SHIFT,7,exec,${pkgs.hyprland}/bin/hyprctl dispatch movetoworkspace 7
    bind = ${modifier}_SHIFT,8,exec,${pkgs.hyprland}/bin/hyprctl dispatch movetoworkspace 8
    bind = ${modifier}_SHIFT,9,exec,${pkgs.hyprland}/bin/hyprctl dispatch movetoworkspace 9
    bind = ${modifier}_SHIFT,0,exec,${pkgs.hyprland}/bin/hyprctl dispatch movetoworkspace 10

    bind = ${modifier},1,exec,${pkgs.hyprland}/bin/hyprctl dispatch focusworkspaceoncurrentmonitor 1
    bind = ${modifier},2,exec,${pkgs.hyprland}/bin/hyprctl dispatch focusworkspaceoncurrentmonitor 2
    bind = ${modifier},3,exec,${pkgs.hyprland}/bin/hyprctl dispatch focusworkspaceoncurrentmonitor 3
    bind = ${modifier},4,exec,${pkgs.hyprland}/bin/hyprctl dispatch focusworkspaceoncurrentmonitor 4
    bind = ${modifier},5,exec,${pkgs.hyprland}/bin/hyprctl dispatch focusworkspaceoncurrentmonitor 5
    bind = ${modifier},6,exec,${pkgs.hyprland}/bin/hyprctl dispatch focusworkspaceoncurrentmonitor 6
    bind = ${modifier},7,exec,${pkgs.hyprland}/bin/hyprctl dispatch focusworkspaceoncurrentmonitor 7
    bind = ${modifier},8,exec,${pkgs.hyprland}/bin/hyprctl dispatch focusworkspaceoncurrentmonitor 8
    bind = ${modifier},9,exec,${pkgs.hyprland}/bin/hyprctl dispatch focusworkspaceoncurrentmonitor 9
    bind = ${modifier},0,exec,${pkgs.hyprland}/bin/hyprctl dispatch focusworkspaceoncurrentmonitor 10

		bindr = ,XF86AudioMute,exec,wpctl set-mute @DEFAULT_SINK@ toggle
		bindr = ,XF86AudioRaiseVolume,exec,wpctl set-volume @DEFAULT_SINK@ 5%+
		bindr = ,XF86AudioLowerVolume,exec,wpctl set-volume @DEFAULT_SINK@ 5%-
		bindr = ,XF86AudioMicMute,exec,wpctl set-mute @DEFAULT_SOURCE@ toggle

		bindr = Ctrl,Up,exec,wpctl set-volume @DEFAULT_SINK@ 5%+
		bindr = Ctrl,Down,exec,wpctl set-volume @DEFAULT_SINK@ 5%-

		bindr = Ctrl,m,exec,wpctl set-mute @DEFAULT_SINK@ toggle
		bindr = Ctrl&Alt,m,exec,wpctl set-mute @DEFAULT_SOURCE@ toggle

    bindl = ,switch:on:[Lid Switch],exec,hyprctl keyword monitor "eDP-1, disable"
    bindl = ,switch:off:[Lid Switch],exec,hyprctl keyword monitor "eDP-1,enable"
    '';
  };

}
