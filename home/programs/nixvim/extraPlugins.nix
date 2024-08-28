{ config, lib, pkgs, ... }:

{
  programs.nixvim.extraPlugins = [
    pkgs.vimPlugins.aerial-nvim
  ];
}
