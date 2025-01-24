{
  config,
  lib,
  pkgs,
  ...
}:
{
  imports = [
    # ./colors.nix
    ./fuzzel.nix
    ./kanshi.nix
    # ./keybindings.nix
    # ./sway.nix
    ./hyprland.nix
    ./hyprlock.nix
    ./swaylock.nix
    ./waybar.nix
    ./eww.nix
    ./wpaperd.nix
    ./catppuccin.nix
  ];
}
