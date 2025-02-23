{ ... }:
let
  wallpaper = "/home/workboots/.dotfiles/assets/wallpapers";
in
{
  services.wpaperd = {
    enable = true;
    settings = {
      default = {
        path = "${wallpaper}";
      };
    };
  };
}
