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
    # ./hyprland.nix
    # ./hyprlock.nix
    # ./swaylock.nix
    # ./sway.nix
    #./waybar.nix
    ./i3.nix
    ./eww.nix
    # ./wpaperd.nix
    ./catppuccin.nix
  ];
}
