{ config, pkgs, lib, ... }:

{
  home.packages = [
    pkgs.statix
    pkgs.python312Packages.flake8

  ];

}
