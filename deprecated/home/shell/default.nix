{ config, pkgs, ... }:

{
  imports = [
    # ./zellij.nix
  ];
  programs.zsh = {
    enable = true;
    syntaxHighlighting = {
      enable = true;
    };
    autosuggestion.enable = true;
    enableCompletion = true;
    shellAliases = {
      "emc" = "emacsclient";
    };
  };
  programs.starship = {
    enable = true;
    enableZshIntegration = true;
    # catppuccin.enable = true;
    settings = {
      directory = {
        truncation_length = 0;
      };
    };
  };
}
