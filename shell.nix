with import <nixpkgs> { };

{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
  name = "jisantuc.github.io";
  buildInputs = [ nodejs-14_x ];
}
