{ ... }:
{
  programs.hyprlock = {
    enable = true;
    sourceFirst = true;
    settings = {
      general = {
        disable_loading_bar = true;
        grace = 0;
        hide_cursor = true;
        no_fade_in = false;
      };

      background = [
        {
          path = "$HOME/.dotfiles/assets/lockscreen-wallpapers/catppuccin-rainbow.png";
          # blur_passes = 1;
          # blur_size = 4;
        }
      ];
      label = [
      {
        text = "$TIME";
        color = "rgb(30, 30, 46)";
        font_size = 100;
        font_family = "Inconsolata Nerd Font Mono";
        position = "0, 200";
        halign = "center";
        valign = "center";
      }
      {
        text = "cmd[update:43200000] date +'%A, %d %B %Y'";
        color = "rgb(30, 30, 46)";
        font_size = 50;
        font_family = "Inconsolata Nerd Font Mono";
        position = "0, 75";
        halign = "center";
        valign = "center";
      }
      ];

      input-field = [
      {
        size = "300, 60";
        outline_thickness = 4;
        dots_size = 0.2;
        dots_spacing = 0.2;
        dots_center = true;
        outer_color = "rgb(24, 24, 37)";
        inner_color = "rgb(49, 50, 68)";
        font-color = "rgb(205, 214, 244)";
        fade_on_empty = false;
        hide_input = false;
        check_color = "rgb(203, 166, 247)";
        fail_color = "rgb(243, 139, 168)";
        fail_text = "Authentication Failed";
        capslock_color = "rgb(250, 179, 135)";
        placeholder_text = "󰌾 Logged in as $USER";
        position = "0, -47";
        halign = "center";
        valign = "center";
      }
      ];
    };
  };
}
