{
  description = "Type and Programming Languages";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils/master";
    nixpkgs.url = "github:nixos/nixpkgs/master";
  };
  
  outputs = inputs@{ self, nixpkgs, ... }:
    with inputs.flake-utils.lib; eachDefaultSystem (system:
      with (import inputs.nixpkgs { inherit system; });
      let when = c: r: if c then r else {};
      in when stdenv.isDarwin (rec {
        packages = flattenTree (recurseIntoAttrs {
          tapl = haskellPackages.callCabal2nix "tapl" ./. {};
        });
        defaultPackage = packages.tapl;
  }));
}
