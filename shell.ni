{ pkgs ? import <nixpkgs> {}}:

pkgs.mkShell {
  packages = [
  pkgs.nixfmt-rfc-style
  pkgs.nil];
}