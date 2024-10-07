{ ... }:
{
  programs.alacritty = {
    enable = true;
    catppuccin.enable = true;
    settings = {
      window = {
        dynamic_title = true;
        dynamic_padding = true;
      };
      font = {
        normal = {
          family = "Inconsolata Nerd Font Mono";
          style = "Regular";
        };
        size = 28;
      };
      # keyboard = {
      #   bindings = [ 
      #     {
      #       action = "ToggleViMode";
      #       key = "Escape";
      #     }
      #   ];
      # };
    };
  };
}

