{ config, pkgs, lib, ... }:

{
  home.packages = [
    pkgs.black
    pkgs.isort
    pkgs.nodePackages.prettier
    pkgs.stylua
    pkgs.shellcheck
    ];

}
