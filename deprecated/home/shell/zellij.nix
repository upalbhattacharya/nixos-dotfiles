{ ... }:
{
  programs.zellij = {
    enable = true;
    # catppuccin.enable = true;
    settings = {
      simplified_ui = true;
      pane_frames = false;
    };
  };
}
