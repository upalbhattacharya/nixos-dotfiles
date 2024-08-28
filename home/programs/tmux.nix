{ config, lib, pkgs, ... }:

{
  programs.tmux = {
    enable = true;
    catppuccin.enable = true;
    plugins = [
    pkgs.tmuxPlugins.continuum
    pkgs.tmuxPlugins.resurrect
    pkgs.tmuxPlugins.sensible
    pkgs.tmuxPlugins.yank
    ];
    extraConfig = ''
    set-window-option -g mode-keys vi
    '';
  };
}
