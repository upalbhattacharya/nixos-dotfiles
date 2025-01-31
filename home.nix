{
  config,
  pkgs,
  catppuccin,
  ...
}:
let
  name = "workboots";
  version = "24.05";
in
{
  imports = [
    ./home
  ];

  nixpkgs.config.allowUnfree = true;

  home = {
    username = "${name}";
    homeDirectory = "/home/${name}";
    stateVersion = "${version}";
    sessionVariables = {

      # session.
      # XDG_CURRENT_DESKTOP = "hyprland";
      # XDG_SESSION_DESKTOP = "hyprland";
      # XDG_SESSION_TYPE = "wayland";

      # wayland stuff.
      # QT_QPA_PLATFORM="wayland";
      # QT_WAYLAND_DISABLE_WINDOWDECORATION="1";
      # SDL_VIDEODRIVER = "wayland";

      # apps stuff.
      _JAVA_AWT_WM_NONREPARENTING = 1;
      WLR_RENDERER = "vulkan";
      WLR_NO_HARDWARE_CURSORS = 1;
      XWAYLAND_NO_GLAMOR = 1;

    };
    packages = [
      pkgs.localsend
      pkgs.zotero
      pkgs.anki-bin
      pkgs.wl-clipboard-rs
      pkgs.ripgrep
      pkgs.jq
      pkgs.socat
    ];
    file = { };
  };
  catppuccin.flavor = "mocha";
  catppuccin.enable = true;
  programs.home-manager.enable = true;

}
