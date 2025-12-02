let
  pkgs = import <nixpkgs> {
    config.allowUnfree = true;
  };
in
pkgs.mkShell {
  packages = with pkgs; [
    dune_3
    ocaml
  ];
}
