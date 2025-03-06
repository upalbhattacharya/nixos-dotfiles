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

      # dwl
      LIBVA_DRIVER_NAME = "nvidia";
      GBM_BACKEND = "nvidia-drm";
      __GLX_VENDOR_LIBRARY_NAME = "nvidia";
      XDG_CURRENT_DESKTOP = "dwl";
      XDG_SESSION_DESKTOP = "dwl";
      QT_AUTO_SCREEN_SCALE_FACTOR = 1;

      # wayland stuff.
      QT_QPA_PLATFORM = "wayland";
      QT_WAYLAND_DISABLE_WINDOWDECORATION = 1;
      SDL_VIDEODRIVER = "wayland";

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
      pkgs.discord
      pkgs.kanshi
      pkgs.graphviz
      pkgs.poetry
      pkgs.python312Full
      pkgs.pandoc
      pkgs.texliveMedium
      pkgs.waydroid
      pkgs.lilypond
      pkgs.frescobaldi
      pkgs.plantuml
      pkgs.cmake
      pkgs.libvterm
      pkgs.libtool
      pkgs.protonvpn-cli_2
      pkgs.emacs30-pgtk
      pkgs.vivaldi
      pkgs.openvpn
      pkgs.ispell
      pkgs.xdg.mime = {
    enable = true;
    defaultApplications = {
      "application/pdf" = "sioyek.desktop";
    };
  };
    ];

    file = { };
  };
  catppuccin.flavor = "mocha";
  catppuccin.enable = true;
  programs.home-manager.enable = true;

}
