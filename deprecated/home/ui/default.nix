{
  config,
  lib,
  pkgs,
  ...
}:
{
  imports = [
    ./fuzzel.nix
    ./kanshi.nix
    ./hyprland.nix
    ./hyprlock.nix
    ./waybar.nix
    # ./wpaperd.nix
    ./catppuccin.nix
  ];
}
